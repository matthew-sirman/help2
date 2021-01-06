//
// Created by matthew on 30/12/2020.
//

#include "../../include/parser/Tokeniser.h"

#include <regex>
#include <numeric>
#include <sstream>
#include <utility>

std::unordered_set<char> Tokeniser::regexEscapeChars = {
        '.', '+', '*', '?', '^', '$', '(', ')', '[', ']', '{', '}', '|', '\\'
};

std::string Tokeniser::infixOpRegex;
std::string Tokeniser::prefixOpRegex;

Tokeniser::Tokeniser(std::string source, Token::LineNumber startLine)
        : source(std::move(source)), startLineNumber(startLine) {

}

Token Tokeniser::startToken() const {
    return Token(source.begin(), source.end(), startLineNumber);
}

bool Tokeniser::tokenImport(Token &token) {
    return matchKeywordToken(token, "import");
}

bool Tokeniser::tokenModule(Token &token) {
    return matchKeywordToken(token, "module");
}

bool Tokeniser::tokenModuleDelimiter(Token &token) {
    return matchSpecifierToken(token, ".");
}

bool Tokeniser::tokenInfix(Token &token) {
    return matchKeywordToken(token, "infix");
}

bool Tokeniser::tokenLeft(Token &token) {
    return matchKeywordToken(token, "left");
}

bool Tokeniser::tokenRight(Token &token) {
    return matchKeywordToken(token, "right");
}

bool Tokeniser::tokenAssociates(Token &token) {
    return matchKeywordToken(token, "associates");
}

bool Tokeniser::tokenVirtual(Token &token) {
    return matchKeywordToken(token, "virtual");
}

bool Tokeniser::tokenFunc(Token &token) {
    return matchKeywordToken(token, "func");
}

bool Tokeniser::tokenValue(Token &token) {
    return matchKeywordToken(token, "value");
}

bool Tokeniser::tokenExprTypeSpecifier(Token &token) {
    return matchSpecifierToken(token, ":");
}

bool Tokeniser::tokenType(Token &token) {
    return matchKeywordToken(token, "type");
}

bool Tokeniser::tokenTypeclass(Token &token) {
    return matchKeywordToken(token, "typeclass");
}

bool Tokeniser::tokenInstance(Token &token) {
    return matchKeywordToken(token, "instance");
}

bool Tokeniser::tokenImplication(Token &token) {
    return matchSpecifierToken(token, "=>");
}

bool Tokeniser::tokenOpenBrace(Token &token) {
    return matchSpecifierToken(token, "{");
}

bool Tokeniser::tokenCloseBrace(Token &token) {
    return matchSpecifierToken(token, "}");
}

bool Tokeniser::tokenDataConstructorSpecifier(Token &token) {
    return matchSpecifierToken(token, "::=");
}

bool Tokeniser::tokenTypeUnion(Token &token) {
    return matchSpecifierToken(token, "|");
}

bool Tokeniser::tokenEndDecl(Token &token) {
    return matchSpecifierToken(token, ";");
}

bool Tokeniser::tokenFuncBody(Token &token) {
    return matchSpecifierToken(token, "=>");
}

bool Tokeniser::tokenValueBody(Token &token) {
    return matchSpecifierToken(token, "=");
}

bool Tokeniser::tokenLet(Token &token) {
    return matchKeywordToken(token, "let");
}

bool Tokeniser::tokenIn(Token &token) {
    return matchKeywordToken(token, "in");
}

bool Tokeniser::tokenLambda(Token &token) {
    return matchSpecifierToken(token, "$");
}

bool Tokeniser::tokenOpenList(Token &token) {
    return matchSpecifierToken(token, "[");
}

bool Tokeniser::tokenComma(Token &token) {
    return matchSpecifierToken(token, ",");
}

bool Tokeniser::tokenCloseList(Token &token) {
    return matchSpecifierToken(token, "]");
}

bool Tokeniser::tokenEmptyList(Token &token) {
    return matchSpecifierToken(token, "[]");
}

bool Tokeniser::tokenConsList(Token &token) {
    return matchSpecifierToken(token, ":");
}

bool Tokeniser::tokenUnit(Token &token) {
    return matchSpecifierToken(token, "()");
}

//bool Tokeniser::tokenSpecialInfixOperator(const Tokeniser &tokeniser, OperatorToken &token) {
//    std::string opName;
//    bool result = matchStringRegexToken(token, tokeniser.infixOpRegex, opName, 1);
//    if (!result) {
//        return false;
//    }
//    token.op = tokeniser.operatorMap.at(opName);
//    return true;
//}
//
//bool Tokeniser::tokenSpecialPrefixOperator(const Tokeniser &tokeniser, OperatorToken &token) {
//    std::string opName;
//    bool result = matchStringRegexToken(token, tokeniser.prefixOpRegex, opName, 1);
//    if (!result) {
//        return false;
//    }
//    token.op = tokeniser.operatorMap.at(opName);
//    return true;
//}

bool Tokeniser::tokenFuncType(Token &token) {
    return matchSpecifierToken(token, "->");
}

bool Tokeniser::tokenOpenParenthesis(Token &token) {
    return matchSpecifierToken(token, "(");
}

bool Tokeniser::tokenCloseParenthesis(Token &token) {
    return matchSpecifierToken(token, ")");
}

bool Tokeniser::tokenIdentifier(IdentifierToken &token) {
    return matchStringRegexToken(token, R"(^\s*([a-zA-Z]\w*'*))", token.identifier, 1);
}

bool Tokeniser::tokenAnyIdentifier(IdentifierToken &token) {
    IdentifierToken::AnyMode type;
    return tokenAnyTrackedIdentifier(token, type);
}

bool Tokeniser::tokenAnyTrackedIdentifier(IdentifierToken &token, IdentifierToken::AnyMode &type) {
    // Check for a normal identifier, then an infix operator, then a prefix operator
    if (tokenIdentifier(token)) {
        type = IdentifierToken::AnyMode::Identifier;
        return true;
    }
    if (matchStringRegexToken(token, prefixOpRegex, token.identifier, 1)) {
        type = IdentifierToken::AnyMode::PrefixOperator;
        return true;
    }
    if (matchStringRegexToken(token, infixOpRegex, token.identifier, 1)) {
        type = IdentifierToken::AnyMode::InfixOperator;
        return true;
    }
    return false;
}

bool Tokeniser::tokenIntegralLiteral(IntegralToken &token) {
    std::string integralString;
    bool result = matchStringRegexToken(token, R"(^\s*(\d+)([^\w\.]|$))", integralString, 1);
    if (!result) {
        return false;
    }
    // This is always safe because we know the string has passed the regex
    token.value = std::stol(integralString);
    return true;
}

bool Tokeniser::tokenDecimalLiteral(DecimalToken &token) {
    std::string decimalString;
    bool result = matchStringRegexToken(token, R"(^\s*(\d+\.\d+)([^\w\.]|$))", decimalString, 1);
    if (!result) {
        return false;
    }
    // This is always safe because we know the string has passed the regex
    token.value = std::stod(decimalString);
    return true;
}

bool Tokeniser::tokenCharLiteral(CharToken &token) {
    std::string charString;
    bool result = matchStringRegexToken(token, R"(^\s*'(\\.|[^\\])')", charString, 1);
    if (!result) {
        return false;
    }
    std::optional<char> unescaped = unescapeChar(charString);
    // The token is only valid if the unescaped character was found
    token.valid = unescaped.has_value();
    // If there was a character found, set the token to contain it
    if (unescaped.has_value()) {
        token.value = unescaped.value();
    }
    return true;
}

bool Tokeniser::tokenStringLiteral(StringToken &token) {
    bool result = matchStringRegexToken(token, R"(^\s*"(([^\\"]|\\.)*)\"($|.))", token.value, 1);
    if (!result) {
        return false;
    }
    std::optional<std::string> unescaped = unescapeString(token.value);
    // The token is only valid if the string unescapes correctly
    token.valid = unescaped.has_value();
    if (unescaped.has_value()) {
        token.value = unescaped.value();
    }
    // Note: if the token is invalid, we don't care what the value is so it is fine to leave it as the original
    // regex match
    return true;
}

PlainToken Tokeniser::scanToken(const Token &token) {
    PlainToken plain(token.position, token.end, token.lineNumber);
    // Simply matches the next "word" (i.e. the next sequence of characters which is not interrupted by any space
    // characters)
    matchStringRegexToken(plain, R"(^\s*(\S*))", plain.token, 1);
    return plain;
}

std::vector<Tokeniser> Tokeniser::findAllStatements() const {
    std::string statement;
    std::vector<Tokeniser> statements;
    Token::LineNumber line = startLineNumber;
    Token::LineNumber startLine = line;
    for (Token::Position it = source.begin(); it != source.end(); it++) {
        // If we see two forward slashes consecutively, then skip over the rest of the line
        // (note the short circuit stops *(it + 1) from ever being invalid)
        if (source.end() - it >= 2 && *it == '/' && *(it + 1) == '/') {
            while (it != source.end() && *++it != '\n') continue;
            // The above will always consume exactly one newline, so we have to increment startLine again.
            // There is one exception: if the line reached the end of file, in which case we don't care about
            // startLine at any further point, so there is no hard in incrementing it.
            line++;
            continue;
        }
        if (*it == '\n') {
            line++;
        }
        if (statement.empty()) {
            startLine = line;
        }
        // Add the current char to the current statement if it is a non-space character, or the statement
        // has been started
        if (!statement.empty() || !isspace(*it)) {
            statement.push_back(*it);
        }
        if (*it == ';') {
            // Add this statement string to the vector
            statements.emplace_back(std::move(statement), startLine);
            startLine = line;
            // Create the next statement object
            statement = std::string();
        }
    }
    return std::move(statements);
}

void Tokeniser::setTokeniserOperatorSet(const OperatorSet &operators) {
    std::stringstream infixRegexBuilder;
    // Start the capture group
    infixRegexBuilder << '(';
    OperatorSet::const_iterator it = operators.begin();
    // Add the first element (if any)
    if (it != operators.end()) {
        infixRegexBuilder << escapeRegex(*it++);
    }
    // Add the rest of the elements, each delimited by a '|'
    while (it != operators.end()) {
        infixRegexBuilder << "|" << escapeRegex(*it++);
    }
    // End the capture group
    infixRegexBuilder << ')';
    infixOpRegex = infixRegexBuilder.str();

    // The prefix regex is the same, only surrounded by another pair of escaped parentheses
    prefixOpRegex = "\\(" + infixOpRegex + "\\)";
}

std::optional<char> Tokeniser::unescapeSwap(char c) {
    switch (c) {
        case 'n':
            return '\n';
        case 'r':
            return '\r';
        case 't':
            return '\t';
        case '\\':
        case '\'':
        case '"':
            return c;
        default:
            return std::nullopt;
    }
}

std::optional<char> Tokeniser::unescapeChar(const std::string &charString) {
    if (charString.front() == '\\') {
        return unescapeSwap(charString[1]);
    }
    return charString.front();
}

std::optional<std::string> Tokeniser::unescapeString(const std::string &string) {
    std::string unescaped;
    // The new string will be at most as big as the original
    unescaped.reserve(string.size());
    for (std::string::const_iterator it = string.begin(); it != string.end(); ++it) {
        // Escape the next character if there is a backslash
        if (*it == '\\') {
            it++;
            // We know by assertion of the above regex that there necessarily must be a following
            // character
            // The following logic is almost identical to the above
            std::optional<char> swap = unescapeSwap(*it);
            if (!swap.has_value()) {
                return std::nullopt;
            }
            unescaped.push_back(swap.value());
            continue;
        }
        // Otherwise just add the character
        unescaped.push_back(*it);
    }
    return unescaped;
}

std::string Tokeniser::escapeRegex(const std::string &string) {
    std::string escaped;
    // Start by hinting that this will be at least as long as the input string
    escaped.reserve(string.size());
    for (std::string::const_iterator it = string.begin(); it != string.end(); ++it) {
        // If the character needs escaping, put a backslash in first
        if (regexEscapeChars.contains(*it)) {
            escaped.push_back('\\');
        }
        // Put the character in
        escaped.push_back(*it);
    }
    return escaped;
}

bool Tokeniser::matchKeywordToken(Token &token, const std::string &keyword) {
    // Match as long as the keyword is some number of space characters, followed by the keyword, followed
    // by a non-word character or the end of the string.
    // Keywords should not contain special characters (?)
    return matchBasicRegexToken(token, "^\\s*(" + keyword + ")(\\W|$)", 1);
}

bool Tokeniser::matchSpecifierToken(Token &token, const std::string &specifier) {
    // Match any number of spaces followed by the specifier. We don't care what comes after the specifier.
    // We also escape the specifier in case it contains special symbols
    return matchBasicRegexToken(token, "^\\s*" + escapeRegex(specifier), 0);
}

bool Tokeniser::matchBasicRegexToken(Token &token, const std::string &search, size_t group) {
    std::string unused;
    return matchStringRegexToken(token, search, unused, group);
}

bool Tokeniser::matchStringRegexToken(Token &token, const std::string &search, std::string &result, size_t group) {
    std::smatch match;
    // Match to the regex
    if (!std::regex_search(token.position, token.end, match, std::regex(search))) {
        // If we didn't match, don't move the position and return false
        return false;
    }
    // Increment the token's line number by the number of newlines seen between it's old and new positions
    token.lineNumber += std::count(token.position, match[group].second, '\n');
    // Update the position in the token to the end of the match group
    token.position = match[group].second;
    // Set the result to the string returned by the group
    result = match[group].str();
    return true;
}
