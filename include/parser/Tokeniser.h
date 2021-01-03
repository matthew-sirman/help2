//
// Created by matthew on 30/12/2020.
//

#ifndef HELPC_TOKENISER_H
#define HELPC_TOKENISER_H

#include <string>
#include <unordered_map>
#include <unordered_set>
#include <optional>
#include <functional>

/*
 * Language Tokens:
 *
 * Infix: infix
 * FuncDecl: func
 * ValueDecl: value
 * ExprTypeSpecifier: ':'
 * TypeDecl: type
 * TypeConstructorSpecifier: '::='
 * TypeUnionSplitter: '|'
 * EndDecl: ';'
 * FuncBodySpecifier: '=>'
 * ValueBodySpecifier: '='
 * LetSpecifier: 'let'
 * InSpecifier: 'in'
 * LambdaSpecifier: '$'
 * Identifier: [a-zA-Z][a-zA-Z0-9_]*'? (e.g. name_Name123')
 * IntegralLiteral: [0-9]+ (e.g. 12345)
 * DecimalLiteral: [0-9]+\.[0-9]+ (e.g. 123.45)
 * StringLiteral: ".*" (e.g. "Hello, World!")
 * OpenList: '['
 * CommaSeparator: ','
 * CloseList: ']'
 * EmptyList: []
 * ConsList: ':'
 * Unit: ()
 * SpecialOperator: '+', '-', '*', '/', '%'
 * FuncType: '->'
 * OpenParenthesis: '('
 * CloseParenthesis: ')'
 */

/*
enum class Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulus
};
*/

class Tokeniser;

struct Token {
    friend class Tokeniser;

    using Position = std::string::const_iterator;
    using LineNumber = std::size_t;

    Position position, end;
    LineNumber lineNumber;

    constexpr bool isEnd() const { return position == end; }

    void update(const Token &other) {
        this->position = other.position;
        this->end = other.end;
        this->lineNumber = other.lineNumber;
    }

protected:
    Token(Position pos, Position end, LineNumber lineNumber) : position(pos), end(end), lineNumber(lineNumber) {}
};

struct IdentifierToken : public Token {
    friend class Tokeniser;

    std::string identifier;

private:
    IdentifierToken(Position pos, Position end, LineNumber lineNumber) : Token(pos, end, lineNumber) {}
};

struct IntegralToken : public Token {
    friend class Tokeniser;

    long long value{};

private:
    IntegralToken(Position pos, Position end, LineNumber lineNumber) : Token(pos, end, lineNumber) {}
};

struct DecimalToken : public Token {
    friend class Tokeniser;

    double value{};

private:
    DecimalToken(Position pos, Position end, LineNumber lineNumber) : Token(pos, end, lineNumber) {}
};

struct CharToken : public Token {
    friend class Tokeniser;

    char value{};
    bool valid{};

private:
    CharToken(Position pos, Position end, LineNumber lineNumber) : Token(pos, end, lineNumber) {}
};

struct StringToken : public Token {
    friend class Tokeniser;

    std::string value;
    bool valid{};

private:
    StringToken(Position pos, Position end, LineNumber lineNumber) : Token(pos, end, lineNumber) {}
};

/*
struct OperatorToken : public Token {
    friend class Tokeniser;

    Operator op{};

private:
    OperatorToken(Position pos, Position end, LineNumber lineNumber) : Token(pos, end, lineNumber) {}
};
*/

struct PlainToken : public Token {
    friend class Tokeniser;

    std::string token;

private:
    explicit PlainToken(Position pos, Position end, LineNumber lineNumber) : Token(pos, end, lineNumber) {}
};

template<typename T>
concept TokenType = std::is_base_of_v<Token, T>;

template<typename FType, typename TType>
concept TokenFunction = requires(FType f, TType t) {
    { f(t) } -> std::convertible_to<bool>;
} && TokenType<TType>;

template<typename FType>
concept AnyTokenFunction = requires {
    TokenFunction<FType, Token> ||
    TokenFunction<FType, IdentifierToken> ||
    TokenFunction<FType, IntegralToken> ||
    TokenFunction<FType, DecimalToken> ||
    TokenFunction<FType, CharToken> ||
    TokenFunction<FType, StringToken>; // ||
    // TokenFunction<FType, OperatorToken>;
};

template<typename T>
struct UnaryFunctionTraits;

template<typename Ret, typename Arg>
struct UnaryFunctionTraits<Ret(Arg)> {
    using ReturnType = Ret;
    using ArgumentType = Arg;
};

template<typename Ret, typename Arg>
struct UnaryFunctionTraits<std::function<Ret(Arg)>> {
    using ReturnType = Ret;
    using ArgumentType = Arg;
};

using OperatorSet = std::unordered_set<std::string>;

/*
 * New Tokeniser with a more functional approach
 * - Allows for contextual token extraction
 */
class Tokeniser {
public:
    explicit Tokeniser(std::string source, Token::LineNumber startLine = 1);

    Token startToken() const;

    template<TokenType _TokTy>
    static _TokTy token(const Token &src);

    // Token: 'import'
    static bool tokenImport(Token &token);

    // Token: '.'
    static bool tokenImportDelimiter(Token &token);

    // Token: 'infix'
    static bool tokenInfix(Token &token);

    // Token: 'left'
    static bool tokenLeft(Token &token);

    // Token: 'right'
    static bool tokenRight(Token &token);

    // Token: 'associates'
    static bool tokenAssociates(Token &token);

    // Token: 'func'
    static bool tokenFunc(Token &token);

    // Token: 'value'
    static bool tokenValue(Token &token);

    // Token: ':'
    static bool tokenExprTypeSpecifier(Token &token);

    // Token: 'type'
    static bool tokenType(Token &token);

    // Token: 'typeclass'
    static bool tokenTypeclass(Token &token);

    // Token: 'instance'
    static bool tokenInstance(Token &token);

    // Token: '=>'
    static bool tokenImplication(Token &token);

    // Token: '{'
    static bool tokenOpenBrace(Token &token);

    // Token: '}'
    static bool tokenCloseBrace(Token &token);

    // Token: '::='
    static bool tokenDataConstructorSpecifier(Token &token);

    // Token: '|'
    static bool tokenTypeUnion(Token &token);

    // Token: ';'
    static bool tokenEndDecl(Token &token);

    // Token: '=>'
    static bool tokenFuncBody(Token &token);

    // Token: '='
    static bool tokenValueBody(Token &token);

    // Token: 'let'
    static bool tokenLet(Token &token);

    // Token: 'in'
    static bool tokenIn(Token &token);

    // Token: '$'
    static bool tokenLambda(Token &token);

    // Token: '['
    static bool tokenOpenList(Token &token);

    // Token: ','
    static bool tokenComma(Token &token);

    // Token: ']'
    static bool tokenCloseList(Token &token);

    // Token: '[]'
    static bool tokenEmptyList(Token &token);

    // Token: ':'
    static bool tokenConsList(Token &token);

    // Token: '()'
    static bool tokenUnit(Token &token);

    /*
    // Token: '+', '-', '*', '/', '%'
    static bool tokenSpecialInfixOperator(const Tokeniser &tokeniser, OperatorToken &token);

    // Token: '(+)', '(-)', '(*)', '(/)', '(%)'
    static bool tokenSpecialPrefixOperator(const Tokeniser &tokeniser, OperatorToken &token);
    */

    // Token: '->'
    static bool tokenFuncType(Token &token);

    // Token: '('
    static bool tokenOpenParenthesis(Token &token);

    // Token: ')'
    static bool tokenCloseParenthesis(Token &token);

    // Token: (re) [a-zA-Z][a-zA-Z0-9_]*'*
    static bool tokenIdentifier(IdentifierToken &token);

    // Token: tokenIdentifier OR any operator
    static bool tokenAnyIdentifier(IdentifierToken &token);

    // Token: (re) [0-9]+
    static bool tokenIntegralLiteral(IntegralToken &token);

    // Token: (re) [0-9]+\.[0-9]+
    static bool tokenDecimalLiteral(DecimalToken &token);

    // Token: (re) '.'
    static bool tokenCharLiteral(CharToken &token);

    // Token: (re) ".*"
    static bool tokenStringLiteral(StringToken &token);

    // Find the next plain token
    static PlainToken scanToken(const Token &token);

    // Search for a particular string
    static bool searchFor(const Token &token, const std::string &string);

    // Composes a set of token scans which must all succeed for the token to be updated. The token
    // must be passable to each of the functions, and so must be the most derived types, and certain
    // compositions will be invalid (e.g. matching an int literal then a decimal literal)
    template<TokenType _TokTy, TokenFunction<_TokTy>... _FTypes>
    static bool compose(_TokTy &token, const _FTypes &...fns);

    // Tries a set of token functions until one succeeds. Otherwise returns false
    template<TokenType _TokTy, TokenFunction<_TokTy>... _FTypes>
    static bool firstOf(_TokTy &token, const _FTypes &...fns);

    // Composes a set of token scans, as with the compose function, but unconditionally resets the token position
    // afterwards.
    template<TokenType _TokTy, AnyTokenFunction... _FTypes>
    static bool peek(const _TokTy &token, const _FTypes &...fns);

    // Tries a set of token scans to see if any match, but unconditionally resets the token position afterwards
    template<TokenType _TokTy, AnyTokenFunction... _FTypes>
    static bool peekAny(const _TokTy &token, const _FTypes &...fns);

    std::vector<Tokeniser> findAllStatements() const;

    static void setTokeniserOperatorSet(const OperatorSet &set);

private:
    static std::optional<char> unescapeSwap(char c);

    static std::optional<char> unescapeChar(const std::string &charString);

    static std::optional<std::string> unescapeString(const std::string &string);

    static std::string escapeRegex(const std::string &string);

    [[maybe_unused]] static std::unordered_set<char> regexEscapeChars;

    static bool matchKeywordToken(Token &token, const std::string &keyword);

    static bool matchSpecifierToken(Token &token, const std::string &specifier);

    static bool matchBasicRegexToken(Token &token, const std::string &search, size_t group = 0);

    static bool matchStringRegexToken(Token &token, const std::string &search, std::string &result, size_t group = 0);

    template<TokenType _TokTy, TokenFunction<_TokTy> _Head, TokenFunction<_TokTy>... _Tail>
    static bool internalCompose(_TokTy &token, const _Head &headFn, const _Tail &...tailFns);

    template<TokenType _TokTy, TokenFunction<_TokTy> _Head>
    static bool internalCompose(_TokTy &token, const _Head &headFn);

    template<TokenType _TokTy, TokenFunction<_TokTy> _Head, TokenFunction<_TokTy>... _Tail>
    static bool internalFirstOf(_TokTy &token, const _Head &headFn, const _Tail &...tailFns);

    template<TokenType _TokTy, TokenFunction<_TokTy> _Head>
    static bool internalFirstOf(_TokTy &token, const _Head &headFn);

    template<TokenType _TokTy, AnyTokenFunction _Head, AnyTokenFunction... _Tail>
    static bool internalPeek(const _TokTy &token, const _Head &headFn, const _Tail &...tailFns);

    template<TokenType _TokTy, AnyTokenFunction _Head>
    static bool internalPeek(const _TokTy &token, const _Head &headFn);

    template<TokenType _TokTy, AnyTokenFunction _Head, AnyTokenFunction ..._Tail>
    static bool internalPeekAny(const _TokTy &token, const _Head &headFn, const _Tail &...tailFns);

    template<TokenType _TokTy, AnyTokenFunction _Head>
    static bool internalPeekAny(const _TokTy &token, const _Head &headFn);

//    template<TokenType _TokTy, AnyTokenFunction _Head> requires (!std::is_base_of_v<typename _Head::argument_type, _TokTy>)
//    static bool internalPeek(_TokTy &token, const _Head &headFn);

    std::string source;
    Token::LineNumber startLineNumber;

    // const OperatorMap &operatorMap;
    static std::string infixOpRegex;
    static std::string prefixOpRegex;
};

template<TokenType _TokTy>
_TokTy Tokeniser::token(const Token &src) {
    return _TokTy(src.position, src.end, src.lineNumber);
}

template<TokenType _TokTy, TokenFunction<_TokTy>... _FTypes>
bool Tokeniser::compose(_TokTy &token, const _FTypes &...fns) {
    // Store the start state of the token
    Token start(token.position, token.end, token.lineNumber);
    // Compose the functions
    bool success = internalCompose(token, fns...);
    // Conditionally reset the position if the scan failed
    if (!success) {
        token.update(start);
    }
    return success;
}

template<TokenType _TokTy, TokenFunction<_TokTy>... _FTypes>
bool Tokeniser::firstOf(_TokTy &token, const _FTypes &...fns) {
    return internalFirstOf(token, fns...);
}

template<TokenType _TokTy, AnyTokenFunction... _FTypes>
bool Tokeniser::peek(const _TokTy &token, const _FTypes &... fns) {
    // Peek into the functions
    return internalPeek(token, fns...);
}

template<TokenType _TokTy, AnyTokenFunction... _FTypes>
bool Tokeniser::peekAny(const _TokTy &token, const _FTypes &...fns) {
    return internalPeekAny(token, fns...);
}

template<TokenType _TokTy, TokenFunction<_TokTy> _Head, TokenFunction<_TokTy>... _Tail>
bool Tokeniser::internalCompose(_TokTy &token, const _Head &headFn,
                                const _Tail &...tailFns) {
    // Short circuit from top (first) down
    if (!headFn(token)) {
        return false;
    }
    // Token is now updated from this call, so we check if the rest were successful.
    return internalCompose(token, tailFns...);
}

template<TokenType _TokTy, TokenFunction<_TokTy> _Head>
bool Tokeniser::internalCompose(_TokTy &token, const _Head &headFn) {
    // In the base case, we can just call the function
    return headFn(token);
}

template<TokenType _TokTy, TokenFunction<_TokTy> _Head, TokenFunction<_TokTy>... _Tail>
bool Tokeniser::internalFirstOf(_TokTy &token, const _Head &headFn, const _Tail &...tailFns) {
    // Short circuit, but this time on success
    if (headFn(token)) {
        return true;
    }
    // Recurse on the tail
    return internalFirstOf(token, tailFns...);
}

template<TokenType _TokTy, TokenFunction<_TokTy> _Head>
bool Tokeniser::internalFirstOf(_TokTy &token, const _Head &headFn) {
    return headFn(token);
}

template<TokenType _TokTy, AnyTokenFunction _Head, AnyTokenFunction... _Tail>
bool Tokeniser::internalPeek(const _TokTy &token, const _Head &headFn, const _Tail &... tailFns) {
    // Short circuit from top down using the base case
    if (!internalPeek(token, headFn)) {
        return false;
    }
    // Call the tail
    return internalPeek(token, tailFns...);
}

template<TokenType _TokTy, AnyTokenFunction _Head>
bool Tokeniser::internalPeek(const _TokTy &token, const _Head &headFn) {
    // In this case, we need to make a new token to pass to the function
    using ArgToken = typename std::remove_reference<typename UnaryFunctionTraits<_Head>::ArgumentType>::type;
    ArgToken newToken = Tokeniser::token<ArgToken>(token);
    return headFn(newToken);
}

template<TokenType _TokTy, AnyTokenFunction _Head, AnyTokenFunction ..._Tail>
bool Tokeniser::internalPeekAny(const _TokTy &token, const _Head &headFn, const _Tail &...tailFns) {
    // Short circuit but on success
    if (internalPeekAny(token, headFn)) {
        return true;
    }
    // Otherwise call the tail
    return internalPeekAny(token, tailFns...);
}

template<TokenType _TokTy, AnyTokenFunction _Head>
bool Tokeniser::internalPeekAny(const _TokTy &token, const _Head &headFn) {
    // In this case, we need to make a new token to pass to the function
    using ArgToken = typename std::remove_reference<typename UnaryFunctionTraits<_Head>::ArgumentType>::type;
    ArgToken newToken = Tokeniser::token<ArgToken>(token);
    return headFn(newToken);
}


#endif //HELPC_TOKENISER_H
