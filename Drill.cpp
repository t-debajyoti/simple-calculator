/*
Simple calculator
Revision history:
Revised by Bjarne Stroustrup (bjarne@stroustrup.com) November 2023
Revised by Bjarne Stroustrup November 2013
Revised by Bjarne Stroustrup May 2007
Revised by Bjarne Stroustrup August 2006
Revised by Bjarne Stroustrup August 2004
Originally written by Bjarne Stroustrup (bs@cs.tamu.edu) Spring 2004.
This program implements a basic expression calculator.
Input from cin; output to cout.
The grammar for input is:

Statement:
Expression
Print
Quit

Print:
";"

Quit:
"q"

Expression:
Term
Expression "+" Term
Expression "-" Term

Term:
Primary
Term "*" Primary
Term "/" Primary
Term "%" Primary

Primary:
Number
"(" Expression ")"
"-" Primary
"+" Primary
"sqrt" "(" Expression ")"
"pow" "(" Expression "," Expression ")"

Number:
floating-point-literal

Input comes from cin through the Token_stream called ts.
*/
#include "std_lib_facilities.h"

class Token
{
public:
    char kind;
    double value;
    string name;
    Token():kind{0} {}  //+,*, ,?,%...
    Token(char ch): kind{ch}, value{0} {}
    Token(char ch,double val): kind{ch}, value(val) { } //Token (number,3.14)
    Token(char ch, string n): kind(ch), name(n) { } //Token (number,"pi")
};

constexpr char quit='Q';
const string quitkey="exit";
constexpr char print=';';
constexpr char prompt='>';
constexpr char result='=';
constexpr char name='a';
constexpr char let='L';
constexpr char declkey='#';
constexpr char square_root='S';
const string sqrtkey="sqrt";
constexpr char power='P';
const string powkey="pow";


class Token_stream
{
public:
    Token get();
    void putback(Token t);
    void ignore(char c);
private:
    bool full {false};
    Token buffer;
};


class Variable
{
public:
    string name;
    double value;
};
vector <Variable> var_table;


void Token_stream::putback(Token t)
{
    if (full) error("putback() into a full buffer");
    buffer = t;
    full = true;
}

constexpr char number='8';

Token Token_stream::get()
{
    if (full)
    {
        full = false;
        return buffer;
    }
    char ch;
    cin >> ch;

    switch (ch)
    {
    //case quit:
    case print:
    case '(':
    case ')':
    case '+':
    case '-':
    case '=':
    case '*':
    case '/':
    case '%':
    case ',':
        return Token{ch};
    case declkey:
        return Token{let};
    case '.':
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
    {
        cin.putback(ch);
        double val;
        cin >> val;
        return Token{number, val};
    }
    default:
        if(isalpha(ch)||ch=='_')
        {
            string s;
            s+=ch;
            while(cin.get(ch) && (isalpha(ch)||isdigit(ch)||ch=='_'))
            {
                s+=ch;
            }
            cin.putback(ch);
            /*if(s==declkey)
            {
                return Token{let};
            }*/
            if(s==sqrtkey)
            {
                return Token{square_root};
            }
            if(s==powkey)
            {
                return Token{power};
            }
            if(s==quitkey)
            {
                return Token{quit};
            }
            return Token{name,s};
        }
        error("Bad Token");
    }
}

void Token_stream::ignore(char c)
{
    if(full && buffer.kind==c)
    {
        full=false;
        return;
    }
    full=false;
    char ch=0;
    while(cin>>ch)
    {
        if(ch==c)
        {
            return;
        }
    }
}

Token_stream ts;

void clean_up_mess()
{
    ts.ignore(print);
}

double expression();
double term();
double declaration();
double get_value(string s);
void set_value(string s, double d);

double primary()
{
    Token t = ts.get();
    switch (t.kind)
    {
    case '(':
    {
        double d = expression();
        t = ts.get();
        if (t.kind != ')') error("')' expected");
        return d;
    }
    case number:
        return t.value;
    case '-':
        return - primary();
    case '+':
        return primary();
    case name:
    {
        Token next=ts.get();    //peeking at the next Token
        if(next.kind=='=')
        {
            double d=expression();
            set_value(t.name,d);
            return d;
        }
        //If not '='
        ts.putback(next);
        return get_value(t.name);
    }
    case square_root:
    {
        t=ts.get();
        if(t.kind!='(')
        {
            error("'(' expected");
        }
        double d=expression();
        t=ts.get();
        if(t.kind!=')')
        {
            error("')' expected");
        }
        if(d<0)
        {
            error("Can not find square root of negative numbers");
        }
        return sqrt(d);
    }
    case power:
    {
        t=ts.get();
        if(t.kind!='(')
        {
            error("'(' expected after 'pow'");
        }
        double x=expression();

        t=ts.get();
        if(t.kind!=',')
        {
            error("',' expected between 'pow' arguments");
        }

        double i_val=expression();
        int i=i_val;
        if(i!=i_val)
        {
            error("Second argument of 'pow' must be an integer...!");
        }

        t=ts.get();
        if(t.kind!=')')
        {
            error("')' expected after 'pow' arguments");
        }
        return pow(x,i);
    }
    default:
        error("primary expected");
    }
}

double get_value(string s)
{
    for(const Variable& v: var_table)
    {
        if(v.name==s)
        {
            return v.value;
        }
    }
    error("Trying to read undefined variable");
}

void set_value(string s,double d)
{
    for(Variable& v: var_table)
    {
        if(v.name==s)
        {
            v.value=d;
            return;
        }
    }
    error("Trying to read undefined variable",s);
}

double statement()
{
    Token t=ts.get();
    switch(t.kind)
    {
    case let:
        return declaration();
    default:
        ts.putback(t);
        return expression();
    }
}

bool is_declared(string var)
{
    for(const Variable& v:var_table)
    {
        if(v.name==var)
        {
            return true;
        }
    }
    return false;
}

double define_name(string var,double val)
{
    /*This portion which stops the variable from being derclared twice is commented out as we updated the code with a re-assignment property to the variable*/

    /*if(is_declared(var))
    {
        error(var," declared twice!");
    }*/
    var_table.push_back(Variable{var,val});
    return val;
}
double declaration()
{
    Token t=ts.get();
    if(t.kind!=name)
    {
        error("name expected in declaration!");
    }
    Token t2=ts.get();
    if(t2.kind!='=')
    {
        error("= expected in declaration of ",t.name);
    }
    double d=expression();
    define_name(t.name,d);
    return d;
}

void calculate()   // Changed to void because we handle quit inside
{
    while (cin)
    {
        try
        {
            cout << prompt;
            Token t = ts.get();
            while (t.kind == print) t = ts.get(); // eat semicolons
            if (t.kind == quit) return;
            ts.putback(t);
            cout << result << statement() << "\n";
        }
        catch (exception& e)
        {
            cerr << e.what() << endl; // print the error
            clean_up_mess();          // discard the bad input
        }
    }
}

int main()
try
{
    define_name("pi",3.1415926535);
    define_name("e",2.7182818284);
    define_name("k",1000);

    calculate();
    keep_window_open("~0");
    return 0;
}

catch (...)
{
    cerr << "exception \n";
    keep_window_open ("~2");
    return 2;
}



double expression()
{
    double left = term();
    Token t = ts.get();
    while(true)
    {
        switch(t.kind)
        {
        case '+':
            left += term();
            t = ts.get();
            break;
        case '-':
            left -= term();
            t = ts.get();
            break;
        default:
            ts.putback(t);
            return left;
        }
    }
}



double term()
{
    double left = primary();
    Token t = ts.get();

    while(true)
    {
        switch (t.kind)
        {
        case '*':
            left *= primary();
            t = ts.get();
            break;
        case '/':
        {
            double d = primary();
            if (d == 0) error("divide by zero");
            left /= d;
            t = ts.get();
            break;
        }
        case '%':
        {
            double d = primary();
            if (d == 0) error("divide by zero");
            left = fmod(left,d);
            t = ts.get();
            break;
        }
        default:
            ts.putback(t);
            return left;
        }
    }
}

