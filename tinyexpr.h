#pragma once

namespace TinyExpr
{
    class Expression
    {
        int type;
        union {double value; const double *bound; const void *function;};
        void *parameters[1];
    };

    enum {
        VARIABLE = 0,

        FUNCTION0 = 8, FUNCTION1, FUNCTION2, FUNCTION3,
        FUNCTION4, FUNCTION5, FUNCTION6, FUNCTION7,

        CLOSURE0 = 16, CLOSURE1, CLOSURE2, CLOSURE3,
        CLOSURE4, CLOSURE5, CLOSURE6, CLOSURE7,

        FLAG_PURE = 32
    } Entity;

    class Variable
    {
        const char *name;
        const void *address;
        int type;
        void *context;
    };

    class Parser
    {
    public:
        Parser();
        
        // Parses the input expression, evaluates it, and frees it.
        // Returns NaN on error.
        double Interpret(const char *expression, int *error);

        // Parses the input expression and binds variables.
        // Returns NULL on error.
        Expression *Compile(const char *expression, const Variable *variables, int var_count, int *error);

        // Evaluates the expression.
        double te_eval(const Expression *n);

        // Prints debugging information on the syntax tree.
        void Print(const Expression *n);

        // Frees the expression.
        // This is safe to call on NULL pointers.
        void Free(Expression *n);
    private:

    };
}