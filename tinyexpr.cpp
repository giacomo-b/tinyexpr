#include "tinyexpr.h"
#include <cmath>
#include <string>
#include <limits>
#include <iostream>

using namespace TinyExpr;

typedef double (*te_fun2)(double, double);

enum {
    TOK_NULL = Entity::CLOSURE7+1, TOK_ERROR, TOK_END, TOK_SEP,
    TOK_OPEN, TOK_CLOSE, TOK_NUMBER, TOK_VARIABLE, TOK_INFIX
};


enum {TE_CONSTANT = 1};


struct State {
    const char *start;
    const char *next;
    int type;
    union {double value; const double *bound; const void *function;};
    void *context;

    const Variable *lookup;
    int lookup_len;
};


#define TYPE_MASK(TYPE) ((TYPE)&0x0000001F)

#define IS_PURE(TYPE) (((TYPE) & Entity::FLAG_PURE) != 0)
#define IS_FUNCTION(TYPE) (((TYPE) & Entity::FUNCTION0) != 0)
#define IS_CLOSURE(TYPE) (((TYPE) & Entity::CLOSURE0) != 0)
#define ARITY(TYPE) ( ((TYPE) & (Entity::FUNCTION0 | Entity::CLOSURE0)) ? ((TYPE) & 0x00000007) : 0 )
#define NEW_EXPR(type, ...) new_expr((type), (const Expression*[]){__VA_ARGS__})

static Expression *new_expr(const int type, const Expression *parameters[]) {
    const int arity = ARITY(type);
    const int psize = sizeof(void*) * arity;
    const int size = (sizeof(Expression) - sizeof(void*)) + psize + (IS_CLOSURE(type) ? sizeof(void*) : 0);
    Expression *ret = malloc(size);
    memset(ret, 0, size);
    if (arity && parameters) {
        memcpy(ret->parameters, parameters, psize);
    }
    ret->type = type;
    ret->bound = 0;
    return ret;
}


void te_free_parameters(Expression *n) {
    if (!n) return;
    switch (TYPE_MASK(n->type)) {
        case Entity::FUNCTION7: case Entity::CLOSURE7: te_free(n->parameters[6]);     /* Falls through. */
        case Entity::FUNCTION6: case Entity::CLOSURE6: te_free(n->parameters[5]);     /* Falls through. */
        case Entity::FUNCTION5: case Entity::CLOSURE5: te_free(n->parameters[4]);     /* Falls through. */
        case Entity::FUNCTION4: case Entity::CLOSURE4: te_free(n->parameters[3]);     /* Falls through. */
        case Entity::FUNCTION3: case Entity::CLOSURE3: te_free(n->parameters[2]);     /* Falls through. */
        case Entity::FUNCTION2: case Entity::CLOSURE2: te_free(n->parameters[1]);     /* Falls through. */
        case Entity::FUNCTION1: case Entity::CLOSURE1: te_free(n->parameters[0]);
    }
}


void te_free(Expression *n) {
    if (!n) return;
    te_free_parameters(n);
    free(n);
}


static double pi(void) {return 3.14159265358979323846;}
static double e(void) {return 2.71828182845904523536;}
static double fac(double a) {/* simplest version of fac */
    if (a < 0.0)
        return std::numeric_limits<double>::quiet_NaN();
    if (a > UINT_MAX)
        return std::numeric_limits<double>::infinity();
    unsigned int ua = (unsigned int)(a);
    unsigned long int result = 1, i;
    for (i = 1; i <= ua; i++) {
        if (i > ULONG_MAX / result)
            return std::numeric_limits<double>::infinity();
        result *= i;
    }
    return (double)result;
}
static double ncr(double n, double r) {
    if (n < 0.0 || r < 0.0 || n < r) return std::numeric_limits<double>::quiet_NaN();
    if (n > UINT_MAX || r > UINT_MAX) return std::numeric_limits<double>::infinity();
    unsigned long int un = (unsigned int)(n), ur = (unsigned int)(r), i;
    unsigned long int result = 1;
    if (ur > un / 2) ur = un - ur;
    for (i = 1; i <= ur; i++) {
        if (result > ULONG_MAX / (un - ur + i))
            return std::numeric_limits<double>::infinity();
        result *= un - ur + i;
        result /= i;
    }
    return result;
}
static double npr(double n, double r) {return ncr(n, r) * fac(r);}

static const Variable functions[] = {
    /* must be in alphabetical order */
    {"abs", fabs,     Entity::FUNCTION1 | Entity::FLAG_PURE, 0},
    {"acos", acos,    Entity::FUNCTION1 | Entity::FLAG_PURE, 0},
    {"asin", asin,    Entity::FUNCTION1 | Entity::FLAG_PURE, 0},
    {"atan", atan,    Entity::FUNCTION1 | Entity::FLAG_PURE, 0},
    {"atan2", atan2,  Entity::FUNCTION2 | Entity::FLAG_PURE, 0},
    {"ceil", ceil,    Entity::FUNCTION1 | Entity::FLAG_PURE, 0},
    {"cos", cos,      Entity::FUNCTION1 | Entity::FLAG_PURE, 0},
    {"cosh", cosh,    Entity::FUNCTION1 | Entity::FLAG_PURE, 0},
    {"e", e,          Entity::FUNCTION0 | Entity::FLAG_PURE, 0},
    {"exp", exp,      Entity::FUNCTION1 | Entity::FLAG_PURE, 0},
    {"fac", fac,      Entity::FUNCTION1 | Entity::FLAG_PURE, 0},
    {"floor", floor,  Entity::FUNCTION1 | Entity::FLAG_PURE, 0},
    {"ln", log,       Entity::FUNCTION1 | Entity::FLAG_PURE, 0},
#ifdef TE_NAT_LOG
    {"log", log,      Entity::FUNCTION1 | Entity::FLAG_PURE, 0},
#else
    {"log", log10,    Entity::FUNCTION1 | Entity::FLAG_PURE, 0},
#endif
    {"log10", log10,  Entity::FUNCTION1 | Entity::FLAG_PURE, 0},
    {"ncr", ncr,      Entity::FUNCTION2 | Entity::FLAG_PURE, 0},
    {"npr", npr,      Entity::FUNCTION2 | Entity::FLAG_PURE, 0},
    {"pi", pi,        Entity::FUNCTION0 | Entity::FLAG_PURE, 0},
    {"pow", pow,      Entity::FUNCTION2 | Entity::FLAG_PURE, 0},
    {"sin", sin,      Entity::FUNCTION1 | Entity::FLAG_PURE, 0},
    {"sinh", sinh,    Entity::FUNCTION1 | Entity::FLAG_PURE, 0},
    {"sqrt", sqrt,    Entity::FUNCTION1 | Entity::FLAG_PURE, 0},
    {"tan", tan,      Entity::FUNCTION1 | Entity::FLAG_PURE, 0},
    {"tanh", tanh,    Entity::FUNCTION1 | Entity::FLAG_PURE, 0},
    {0, 0, 0, 0}
};

static const Variable *find_builtin(const char *name, int len) {
    int imin = 0;
    int imax = sizeof(functions) / sizeof(Variable) - 2;

    /*Binary search.*/
    while (imax >= imin) {
        const int i = (imin + ((imax-imin)/2));
        int c = strncmp(name, functions[i].name, len);
        if (!c) c = '\0' - functions[i].name[len];
        if (c == 0) {
            return functions + i;
        } else if (c > 0) {
            imin = i + 1;
        } else {
            imax = i - 1;
        }
    }

    return 0;
}

static const Variable *find_lookup(const state *s, const char *name, int len) {
    int iters;
    const Variable *var;
    if (!s->lookup) return 0;

    for (var = s->lookup, iters = s->lookup_len; iters; ++var, --iters) {
        if (strncmp(name, var->name, len) == 0 && var->name[len] == '\0') {
            return var;
        }
    }
    return 0;
}



static double add(double a, double b) {return a + b;}
static double sub(double a, double b) {return a - b;}
static double mul(double a, double b) {return a * b;}
static double divide(double a, double b) {return a / b;}
static double negate(double a) {return -a;}
static double comma(double a, double b) {(void)a; return b;}


void next_token(state *s) {
    s->type = TOK_NULL;

    do {

        if (!*s->next){
            s->type = TOK_END;
            return;
        }

        /* Try reading a number. */
        if ((s->next[0] >= '0' && s->next[0] <= '9') || s->next[0] == '.') {
            s->value = strtod(s->next, (char**)&s->next);
            s->type = TOK_NUMBER;
        } else {
            /* Look for a variable or builtin function call. */
            if (s->next[0] >= 'a' && s->next[0] <= 'z') {
                const char *start;
                start = s->next;
                while ((s->next[0] >= 'a' && s->next[0] <= 'z') || (s->next[0] >= '0' && s->next[0] <= '9') || (s->next[0] == '_')) s->next++;

                const Variable *var = find_lookup(s, start, s->next - start);
                if (!var) var = find_builtin(start, s->next - start);

                if (!var) {
                    s->type = TOK_ERROR;
                } else {
                    switch(TYPE_MASK(var->type))
                    {
                        case Entity::VARIABLE:
                            s->type = TOK_VARIABLE;
                            s->bound = var->address;
                            break;

                        case Entity::CLOSURE0: case Entity::CLOSURE1: case Entity::CLOSURE2: case Entity::CLOSURE3:         /* Falls through. */
                        case Entity::CLOSURE4: case Entity::CLOSURE5: case Entity::CLOSURE6: case Entity::CLOSURE7:         /* Falls through. */
                            s->context = var->context;                                                  /* Falls through. */

                        case Entity::FUNCTION0: case Entity::FUNCTION1: case Entity::FUNCTION2: case Entity::FUNCTION3:     /* Falls through. */
                        case Entity::FUNCTION4: case Entity::FUNCTION5: case Entity::FUNCTION6: case Entity::FUNCTION7:     /* Falls through. */
                            s->type = var->type;
                            s->function = var->address;
                            break;
                    }
                }

            } else {
                /* Look for an operator or special character. */
                switch (s->next++[0]) {
                    case '+': s->type = TOK_INFIX; s->function = add; break;
                    case '-': s->type = TOK_INFIX; s->function = sub; break;
                    case '*': s->type = TOK_INFIX; s->function = mul; break;
                    case '/': s->type = TOK_INFIX; s->function = divide; break;
                    case '^': s->type = TOK_INFIX; s->function = pow; break;
                    case '%': s->type = TOK_INFIX; s->function = fmod; break;
                    case '(': s->type = TOK_OPEN; break;
                    case ')': s->type = TOK_CLOSE; break;
                    case ',': s->type = TOK_SEP; break;
                    case ' ': case '\t': case '\n': case '\r': break;
                    default: s->type = TOK_ERROR; break;
                }
            }
        }
    } while (s->type == TOK_NULL);
}


static Expression *list(state *s);
static Expression *expr(state *s);
static Expression *power(state *s);

static Expression *base(state *s) {
    /* <base>      =    <constant> | <variable> | <function-0> {"(" ")"} | <function-1> <power> | <function-X> "(" <expr> {"," <expr>} ")" | "(" <list> ")" */
    Expression *ret;
    int arity;

    switch (TYPE_MASK(s->type)) {
        case TOK_NUMBER:
            ret = new_expr(TE_CONSTANT, 0);
            ret->value = s->value;
            next_token(s);
            break;

        case TOK_VARIABLE:
            ret = new_expr(Entity::VARIABLE, 0);
            ret->bound = s->bound;
            next_token(s);
            break;

        case Entity::FUNCTION0:
        case Entity::CLOSURE0:
            ret = new_expr(s->type, 0);
            ret->function = s->function;
            if (IS_CLOSURE(s->type)) ret->parameters[0] = s->context;
            next_token(s);
            if (s->type == TOK_OPEN) {
                next_token(s);
                if (s->type != TOK_CLOSE) {
                    s->type = TOK_ERROR;
                } else {
                    next_token(s);
                }
            }
            break;

        case Entity::FUNCTION1:
        case Entity::CLOSURE1:
            ret = new_expr(s->type, 0);
            ret->function = s->function;
            if (IS_CLOSURE(s->type)) ret->parameters[1] = s->context;
            next_token(s);
            ret->parameters[0] = power(s);
            break;

        case Entity::FUNCTION2: case Entity::FUNCTION3: case Entity::FUNCTION4:
        case Entity::FUNCTION5: case Entity::FUNCTION6: case Entity::FUNCTION7:
        case Entity::CLOSURE2: case Entity::CLOSURE3: case Entity::CLOSURE4:
        case Entity::CLOSURE5: case Entity::CLOSURE6: case Entity::CLOSURE7:
            arity = ARITY(s->type);

            ret = new_expr(s->type, 0);
            ret->function = s->function;
            if (IS_CLOSURE(s->type)) ret->parameters[arity] = s->context;
            next_token(s);

            if (s->type != TOK_OPEN) {
                s->type = TOK_ERROR;
            } else {
                int i;
                for(i = 0; i < arity; i++) {
                    next_token(s);
                    ret->parameters[i] = expr(s);
                    if(s->type != TOK_SEP) {
                        break;
                    }
                }
                if(s->type != TOK_CLOSE || i != arity - 1) {
                    s->type = TOK_ERROR;
                } else {
                    next_token(s);
                }
            }

            break;

        case TOK_OPEN:
            next_token(s);
            ret = list(s);
            if (s->type != TOK_CLOSE) {
                s->type = TOK_ERROR;
            } else {
                next_token(s);
            }
            break;

        default:
            ret = new_expr(0, 0);
            s->type = TOK_ERROR;
            ret->value = std::numeric_limits<double>::quiet_NaN();
            break;
    }

    return ret;
}


static Expression *power(state *s) {
    /* <power>     =    {("-" | "+")} <base> */
    int sign = 1;
    while (s->type == TOK_INFIX && (s->function == add || s->function == sub)) {
        if (s->function == sub) sign = -sign;
        next_token(s);
    }

    Expression *ret;

    if (sign == 1) {
        ret = base(s);
    } else {
        ret = NEW_EXPR(Entity::FUNCTION1 | Entity::FLAG_PURE, base(s));
        ret->function = negate;
    }

    return ret;
}

#ifdef TE_POW_FROM_RIGHT
static Expression *factor(state *s) {
    /* <factor>    =    <power> {"^" <power>} */
    Expression *ret = power(s);

    int neg = 0;

    if (ret->type == (Entity::FUNCTION1 | Entity::FLAG_PURE) && ret->function == negate) {
        Expression *se = ret->parameters[0];
        free(ret);
        ret = se;
        neg = 1;
    }

    Expression *insertion = 0;

    while (s->type == TOK_INFIX && (s->function == pow)) {
        te_fun2 t = s->function;
        next_token(s);

        if (insertion) {
            /* Make exponentiation go right-to-left. */
            Expression *insert = NEW_EXPR(Entity::FUNCTION2 | Entity::FLAG_PURE, insertion->parameters[1], power(s));
            insert->function = t;
            insertion->parameters[1] = insert;
            insertion = insert;
        } else {
            ret = NEW_EXPR(Entity::FUNCTION2 | Entity::FLAG_PURE, ret, power(s));
            ret->function = t;
            insertion = ret;
        }
    }

    if (neg) {
        ret = NEW_EXPR(Entity::FUNCTION1 | Entity::FLAG_PURE, ret);
        ret->function = negate;
    }

    return ret;
}
#else
static Expression *factor(state *s) {
    /* <factor>    =    <power> {"^" <power>} */
    Expression *ret = power(s);

    while (s->type == TOK_INFIX && (s->function == pow)) {
        te_fun2 t = s->function;
        next_token(s);
        ret = NEW_EXPR(Entity::FUNCTION2 | Entity::FLAG_PURE, ret, power(s));
        ret->function = t;
    }

    return ret;
}
#endif



static Expression *term(state *s) {
    /* <term>      =    <factor> {("*" | "/" | "%") <factor>} */
    Expression *ret = factor(s);

    while (s->type == TOK_INFIX && (s->function == mul || s->function == divide || s->function == fmod)) {
        te_fun2 t = s->function;
        next_token(s);
        ret = NEW_EXPR(Entity::FUNCTION2 | Entity::FLAG_PURE, ret, factor(s));
        ret->function = t;
    }

    return ret;
}


static Expression *expr(state *s) {
    /* <expr>      =    <term> {("+" | "-") <term>} */
    Expression *ret = term(s);

    while (s->type == TOK_INFIX && (s->function == add || s->function == sub)) {
        te_fun2 t = s->function;
        next_token(s);
        ret = NEW_EXPR(Entity::FUNCTION2 | Entity::FLAG_PURE, ret, term(s));
        ret->function = t;
    }

    return ret;
}


static Expression *list(state *s) {
    /* <list>      =    <expr> {"," <expr>} */
    Expression *ret = expr(s);

    while (s->type == TOK_SEP) {
        next_token(s);
        ret = NEW_EXPR(Entity::FUNCTION2 | Entity::FLAG_PURE, ret, expr(s));
        ret->function = comma;
    }

    return ret;
}


#define TE_FUN(...) ((double(*)(__VA_ARGS__))n->function)
#define M(e) te_eval(n->parameters[e])


double te_eval(const Expression *n) {
    if (!n) return std::numeric_limits<double>::quiet_NaN();

    switch(TYPE_MASK(n->type)) {
        case TE_CONSTANT: return n->value;
        case Entity::VARIABLE: return *n->bound;

        case Entity::FUNCTION0: case Entity::FUNCTION1: case Entity::FUNCTION2: case Entity::FUNCTION3:
        case Entity::FUNCTION4: case Entity::FUNCTION5: case Entity::FUNCTION6: case Entity::FUNCTION7:
            switch(ARITY(n->type)) {
                case 0: return TE_FUN(void)();
                case 1: return TE_FUN(double)(M(0));
                case 2: return TE_FUN(double, double)(M(0), M(1));
                case 3: return TE_FUN(double, double, double)(M(0), M(1), M(2));
                case 4: return TE_FUN(double, double, double, double)(M(0), M(1), M(2), M(3));
                case 5: return TE_FUN(double, double, double, double, double)(M(0), M(1), M(2), M(3), M(4));
                case 6: return TE_FUN(double, double, double, double, double, double)(M(0), M(1), M(2), M(3), M(4), M(5));
                case 7: return TE_FUN(double, double, double, double, double, double, double)(M(0), M(1), M(2), M(3), M(4), M(5), M(6));
                default: return std::numeric_limits<double>::quiet_NaN();
            }

        case Entity::CLOSURE0: case Entity::CLOSURE1: case Entity::CLOSURE2: case Entity::CLOSURE3:
        case Entity::CLOSURE4: case Entity::CLOSURE5: case Entity::CLOSURE6: case Entity::CLOSURE7:
            switch(ARITY(n->type)) {
                case 0: return TE_FUN(void*)(n->parameters[0]);
                case 1: return TE_FUN(void*, double)(n->parameters[1], M(0));
                case 2: return TE_FUN(void*, double, double)(n->parameters[2], M(0), M(1));
                case 3: return TE_FUN(void*, double, double, double)(n->parameters[3], M(0), M(1), M(2));
                case 4: return TE_FUN(void*, double, double, double, double)(n->parameters[4], M(0), M(1), M(2), M(3));
                case 5: return TE_FUN(void*, double, double, double, double, double)(n->parameters[5], M(0), M(1), M(2), M(3), M(4));
                case 6: return TE_FUN(void*, double, double, double, double, double, double)(n->parameters[6], M(0), M(1), M(2), M(3), M(4), M(5));
                case 7: return TE_FUN(void*, double, double, double, double, double, double, double)(n->parameters[7], M(0), M(1), M(2), M(3), M(4), M(5), M(6));
                default: return std::numeric_limits<double>::quiet_NaN();
            }

        default: return std::numeric_limits<double>::quiet_NaN();
    }

}

#undef TE_FUN
#undef M

static void optimize(Expression *n) {
    /* Evaluates as much as possible. */
    if (n->type == TE_CONSTANT) return;
    if (n->type == Entity::VARIABLE) return;

    /* Only optimize out functions flagged as pure. */
    if (IS_PURE(n->type)) {
        const int arity = ARITY(n->type);
        int known = 1;
        int i;
        for (i = 0; i < arity; ++i) {
            optimize(n->parameters[i]);
            if (((Expression*)(n->parameters[i]))->type != TE_CONSTANT) {
                known = 0;
            }
        }
        if (known) {
            const double value = te_eval(n);
            te_free_parameters(n);
            n->type = TE_CONSTANT;
            n->value = value;
        }
    }
}


Expression *te_compile(const char *expression, const Variable *variables, int var_count, int *error) {
    state s;
    s.start = s.next = expression;
    s.lookup = variables;
    s.lookup_len = var_count;

    next_token(&s);
    Expression *root = list(&s);

    if (s.type != TOK_END) {
        te_free(root);
        if (error) {
            *error = (s.next - s.start);
            if (*error == 0) *error = 1;
        }
        return 0;
    } else {
        optimize(root);
        if (error) *error = 0;
        return root;
    }
}


double te_interp(const char *expression, int *error) {
    Expression *n = te_compile(expression, 0, 0, error);
    double ret;
    if (n) {
        ret = te_eval(n);
        te_free(n);
    } else {
        ret = std::numeric_limits<double>::quiet_NaN();
    }
    return ret;
}

static void pn (const Expression *n, int depth) {
    int i, arity;
    printf("%*s", depth, "");

    switch(TYPE_MASK(n->type)) {
    case TE_CONSTANT: printf("%f\n", n->value); break;
    case Entity::VARIABLE: printf("bound %p\n", n->bound); break;

    case Entity::FUNCTION0: case Entity::FUNCTION1: case Entity::FUNCTION2: case Entity::FUNCTION3:
    case Entity::FUNCTION4: case Entity::FUNCTION5: case Entity::FUNCTION6: case Entity::FUNCTION7:
    case Entity::CLOSURE0: case Entity::CLOSURE1: case Entity::CLOSURE2: case Entity::CLOSURE3:
    case Entity::CLOSURE4: case Entity::CLOSURE5: case Entity::CLOSURE6: case Entity::CLOSURE7:
         arity = ARITY(n->type);
         printf("f%d", arity);
         for(i = 0; i < arity; i++) {
             printf(" %p", n->parameters[i]);
         }
         printf("\n");
         for(i = 0; i < arity; i++) {
             pn(n->parameters[i], depth + 1);
         }
         break;
    }
}


void Parser::Print(const Expression *n) {
    pn(n, 0);
}