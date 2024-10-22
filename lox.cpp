#include <cassert>
#include <cctype>
#include <fstream>
#include <iostream>
#include <optional>
#include <sstream>
#include <string>
#include <sysexits.h>
#include <variant>
#include <vector>

// =====
// utils
std::string str_from_file(const char* filename) {
    std::ifstream ifs(filename, std::ios::in | std::ios::binary | std::ios::ate);

    std::ifstream::pos_type fileSize = ifs.tellg();
    ifs.seekg(0, std::ios::beg);

    std::vector<char> bytes(fileSize);
    ifs.read(bytes.data(), fileSize);

    return std::string(bytes.data(), fileSize);
}

std::string str_slice(const std::string& str, size_t start, size_t end) {
    return str.substr(start, end - start);
}

// =====
// error template
/*
    struct Error {
        int line;
        std::string str() const {
            std::ostringstream oss;

            oss << "ERROR [_stage_]: ";
            oss << "on line " << line;

            return oss.str();
        }
    };
    std::vector<Error> errors;

 */


// =====
// lox
template <typename Error>
void report_errors(
    const std::vector<Error>& errors,
    const std::vector<std::string>& lines
) {
    for (const auto& error : errors) {
        std::cerr << error.str() << std::endl;
        int line_num = error.line;

        if (line_num - 1 >= 1)  {
            std::cerr << '\t' << std::setw(5) << (line_num - 1) << "| " << lines[line_num - 2] << std::endl;
        }
        std::cerr << '\t' << std::setw(5) << line_num << "| " << lines[line_num - 1] << "   <<<<<<<<<< " << std::endl;
        if (line_num + 1 <= lines.size())  {
            std::cerr << '\t' << std::setw(5) << (line_num + 1) << "| " << lines[line_num] << std::endl;
        }
        std::cerr << std::endl;
    }
}

struct Token {
    enum class Type {
        // Single-character tokens
        LEFT_PAREN, RIGHT_PAREN,
        LEFT_BRACE, RIGHT_BRACE,
        COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR,

        // 1-2 character tokens
        BANG, BANG_EQUAL,
        EQUAL, EQUAL_EQUAL,
        GREATER, GREATER_EQUAL,
        LESS, LESS_EQUAL,

        // literals
        IDENTIFIER, STRING, NUMBER, TRUE, FALSE, NIL,

        // keywords
        AND,
        CLASS,
        ELSE,
        FUN,
        FOR,
        IF,
        OR,
        PRINT,
        RETURN,
        SUPER,
        THIS,
        VAR,
        WHILE,

        //
        EOF_TOKEN
    };

    static std::string type_to_str(Type type) {
        switch (type) {
        case Type::LEFT_PAREN: return "LEFT_PAREN";
        case Type::RIGHT_PAREN: return "RIGHT_PAREN";
        case Type::LEFT_BRACE: return "LEFT_BRACE";
        case Type::RIGHT_BRACE: return "RIGHT_BRACE";
        case Type::COMMA: return "COMMA";
        case Type::DOT: return "DOT";
        case Type::MINUS: return "MINUS";
        case Type::PLUS: return "PLUS";
        case Type::SEMICOLON: return "SEMICOLON";
        case Type::SLASH: return "SLASH";
        case Type::STAR: return "STAR";
        case Type::BANG: return "BANG";
        case Type::BANG_EQUAL: return "BANG_EQUAL";
        case Type::EQUAL: return "EQUAL";
        case Type::EQUAL_EQUAL: return "EQUAL_EQUAL";
        case Type::GREATER: return "GREATER";
        case Type::GREATER_EQUAL: return "GREATER_EQUAL";
        case Type::LESS: return "LESS";
        case Type::LESS_EQUAL: return "LESS_EQUAL";
        case Type::IDENTIFIER: return "IDENTIFIER";
        case Type::STRING: return "STRING";
        case Type::NUMBER: return "NUMBER";
        case Type::AND: return "AND";
        case Type::CLASS: return "CLASS";
        case Type::ELSE: return "ELSE";
        case Type::FALSE: return "FALSE";
        case Type::FUN: return "FUN";
        case Type::FOR: return "FOR";
        case Type::IF: return "IF";
        case Type::NIL: return "NIL";
        case Type::OR: return "OR";
        case Type::PRINT: return "PRINT";
        case Type::RETURN: return "RETURN";
        case Type::SUPER: return "SUPER";
        case Type::TRUE: return "TRUE";
        case Type::VAR: return "VAR";
        case Type::WHILE: return "WHILE";
        case Type::EOF_TOKEN: return "EOF_TOKEN";
        default: return std::to_string((int)type);
        }
    }

    int line;
    Type type;
    using Value = std::variant<nullptr_t, double, std::string>;
    Value value;

    Token(int line, Type type)
        : line(line)
        , type(type)
        , value(nullptr)
    {
    }

    Token(int line, Type type, double value)
        : line(line)
        , type(type)
        , value(value)
    {
    }

    Token(int line, Type type, const std::string& value)
        : line(line)
        , type(type)
        , value(value)
    {
    }

    bool is_literal_type() const {
        return type == Type::STRING
            || type == Type::NUMBER
            || type == Type::TRUE
            || type == Type::FALSE
            || type == Type::NIL;
    }

    std::string str() const {
        std::ostringstream oss;
        oss << "<Token"
            << " line=" << line
            << " type=" << type_to_str(type);
        if (std::holds_alternative<double>(value)) {
            oss << " value=" << std::get<double>(value);
        } else if (std::holds_alternative<std::string>(value)) {
            if (type == Type::STRING) oss << " value=\"" << std::get<std::string>(value) << '"';
            else oss << " value=" << std::get<std::string>(value);
        }
        oss << ">";
        return oss.str();
    }
};

class Scanner {
public:
    struct Error {
        int line;
        enum class Type {
            UNKNOWN,
            UNTERMINATED_STRING,
        } type;

        std::string str() const {
            std::ostringstream oss;
            oss << "ERROR [Scanner]: ";

            switch (type) {
            case Type::UNTERMINATED_STRING:
                oss << "[UNTERMINATED_STRING] Unterminated string ";
                break;
            default:
                oss << "[UNKNOWN] Unknown error ";
                break;
            }
            oss << "on line " << line;

            return oss.str();
        }
    };

    std::vector<Token> tokens;
    std::vector<Error> errors;
    std::vector<std::string> lines;
    int line = 1;

private:
    enum class State {
        DEFAULT,
        COMMENT,
        STRING,
        NUMBER,
        KEYWORD_OR_IDENTIFIER
    } state = State::DEFAULT;

    const std::string& code;
    size_t index = 0;
    size_t start_line = 1;

    void add_token(Token::Type token_type) {
        tokens.emplace_back(Token(line, token_type));
    }

    void add_token(Token::Type token_type, double value) {
        tokens.emplace_back(Token(line, token_type, value));
    }

    void add_token(Token::Type token_type, const std::string& value) {
        tokens.emplace_back(Token(line, token_type, value));
    }

    inline bool is_keyword_or_identifier_start(char c) {
        return
            c == '_' ||
            c == '$' ||
            std::isalpha(c);
    }

    void handle_state_default() {
        switch (code[index]) {
        // ignore
        case '\r':
        case '\n':
        case '\t':
        case ' ':
            ++index;
            break;

        // unambiguous single character tokens
        case '(': add_token(Token::Type::LEFT_PAREN);  ++index; break;
        case ')': add_token(Token::Type::RIGHT_PAREN); ++index; break;
        case '{': add_token(Token::Type::LEFT_BRACE);  ++index; break;
        case '}': add_token(Token::Type::RIGHT_BRACE); ++index; break;
        case ',': add_token(Token::Type::COMMA);       ++index; break;
        case '.': add_token(Token::Type::DOT);         ++index; break;
        case '-': add_token(Token::Type::MINUS);       ++index; break;
        case '+': add_token(Token::Type::PLUS);        ++index; break;
        case ';': add_token(Token::Type::SEMICOLON);   ++index; break;
        case '*': add_token(Token::Type::STAR);        ++index; break;

        // ambiguous 1+ character tokens
        case '!':
            if (index+1 < code.size() && code[index+1] == '=') {
                add_token(Token::Type::BANG_EQUAL);
                index += 2;
            } else {
                add_token(Token::Type::BANG);
                index += 1;
            }
            break;
        case '=':
            if (index+1 < code.size() && code[index+1] == '=') {
                add_token(Token::Type::EQUAL_EQUAL);
                index += 2;
            } else {
                add_token(Token::Type::EQUAL);
                index += 1;
            }
            break;
        case '>':
            if (index+1 < code.size() && code[index+1] == '=') {
                add_token(Token::Type::GREATER_EQUAL);
                index += 2;
            } else {
                add_token(Token::Type::GREATER);
                index += 1;
            }
            break;
        case '<':
            if (index+1 < code.size() && code[index+1] == '=') {
                add_token(Token::Type::LESS_EQUAL);
                index += 2;
            } else {
                add_token(Token::Type::LESS);
                index += 1;
            }
            break;
        case '/':
            if (index+1 < code.size() && code[index+1] == '/') {
                state = State::COMMENT;
                index += 2;
            } else {
                add_token(Token::Type::SLASH);
                index += 1;
            }
            break;
        case '"':
            state = State::STRING;
            ++index;
            break;

        default:
            if (std::isdigit(code[index])) {
                state = State::NUMBER;
            } else if (is_keyword_or_identifier_start(code[index])) {
                state = State::KEYWORD_OR_IDENTIFIER;
            } else {
                std::cerr << "Unexpected character at [" << index << "]: " << code[index] << std::endl;
                ++index;
            }
            break;
        }
    }

    void handle_state_comment() {
        if (code[index] == '\n') {
            state = State::DEFAULT;
        }
        ++index;
    }

    void handle_state_string() {
        size_t start = index;
        start_line = line;
        while (true) {
            if (code[index] == '"') {
                std::string word = str_slice(code, start, index);
                add_token(Token::Type::STRING, word);
                state = State::DEFAULT;
                ++index;
                return;
            }

            ++index;
            if (index >= code.size()) {
                return;
            }
        }
    }

    inline bool valid_number_character(char c) {
        return
            c == '.' ||
            c == '_' ||
            std::isdigit(code[index]);
    }

    void handle_state_number() {
        size_t start = index;
        start_line = line;
        while (true) {
            if (index >= code.size() || !valid_number_character(code[index])) {
                std::string double_str = str_slice(code, start, index);
                double value = std::stod(double_str);
                add_token(Token::Type::NUMBER, value);
                state = State::DEFAULT;
                return;
            }
            ++index;
        }
    }


    inline bool valid_keyword_or_identifier_character(char c) {
        return std::isdigit(c) || is_keyword_or_identifier_start(c);
    }

    Token::Type word_to_token_type(const std::string& word) {
        if (word == "and") return Token::Type::AND;
        if (word == "class") return Token::Type::CLASS;
        if (word == "else") return Token::Type::ELSE;
        if (word == "false") return Token::Type::FALSE;
        if (word == "fun") return Token::Type::FUN;
        if (word == "for") return Token::Type::FOR;
        if (word == "if") return Token::Type::IF;
        if (word == "nil") return Token::Type::NIL;
        if (word == "or") return Token::Type::OR;
        if (word == "print") return Token::Type::PRINT;
        if (word == "return") return Token::Type::RETURN;
        if (word == "super") return Token::Type::SUPER;
        if (word == "this") return Token::Type::THIS;
        if (word == "true") return Token::Type::TRUE;
        if (word == "var") return Token::Type::VAR;
        if (word == "while") return Token::Type::WHILE;

        return Token::Type::IDENTIFIER;
    }

    void handle_state_keyword_or_identifier() {
        size_t start = index;
        start_line = line;
        while (true) {
            if (index >= code.size() || !valid_keyword_or_identifier_character(code[index])) {
                std::string word = str_slice(code, start, index);
                Token::Type token_type = word_to_token_type(word);
                add_token(token_type, word);
                state = State::DEFAULT;
                return;
            }
            ++index;
        }
    }

    void advance() {
        switch (state) {
        case State::COMMENT: handle_state_comment(); break;
        case State::STRING: handle_state_string(); break;
        case State::NUMBER: handle_state_number(); break;
        case State::KEYWORD_OR_IDENTIFIER: handle_state_keyword_or_identifier(); break;
        default: handle_state_default(); break;
        }
    }

    void scan_tokens() {
        tokens = {};
        errors = {};
        index = 0;
        line = 1;
        start_line = 1;

        while (index < code.size()) {
            if (code[index] == '\n') {
                ++line;
            }
            advance();
        }

        if (state == State::STRING) {
            Error error;
            error.line = start_line;
            error.type = Error::Type::UNTERMINATED_STRING;
            errors.emplace_back(error);
        }
    }

    void read_lines() {
        std::istringstream codess(code);
        for (std::string l; std::getline(codess, l, '\n');) {
            lines.emplace_back(l);
        }
    }

public:
    Scanner(const std::string& code)
        : code(code)
    {
        read_lines();
        scan_tokens();
    }

    bool valid() const {
        return errors.empty();
    }
};

namespace Literal {
    using Literal = std::variant<nullptr_t, double, std::string, bool>;

    std::string to_string(const Literal& literal) {
        if (std::holds_alternative<nullptr_t>(literal)) return "nil";
        if (std::holds_alternative<double>(literal)) return std::to_string(std::get<double>(literal));
        if (std::holds_alternative<std::string>(literal)) return std::get<std::string>(literal);
        if (std::holds_alternative<bool>(literal)) {
            bool value = std::get<bool>(literal);
            return value
                ? "true"
                : "false";
        }
        return "@@UNKL@@";
    }

    std::optional<Literal> from_token_value(const Token::Value& token_value) {
        if (std::holds_alternative<nullptr_t>(token_value)) return nullptr;
        if (std::holds_alternative<double>(token_value)) return std::get<double>(token_value);
        if (std::holds_alternative<std::string>(token_value)) return std::get<std::string>(token_value);
        return std::nullopt;
    }
}

struct Expr {
    enum class Op {
        // binary
        IS_EQUAL, IS_NOT_EQUAL,
        LESS_THAN, LESS_THAN_OR_EQUAL_TO,
        GREATER_THAN, GREATER_THAN_OR_EQUAL_TO,
        SUBTRACT, ADD,
        DIVIDE, MULTIPLY,

        // unary
        NEGATE, NOT,

        // nullary
        LITERAL,
    } op;
    std::vector<Expr*> children;
    Literal::Literal literal;

    static std::string op_to_str(Op op) {
        switch (op) {
        case Op::IS_EQUAL: return "IS_EQUAL";
        case Op::IS_NOT_EQUAL: return "IS_NOT_EQUAL";
        case Op::LESS_THAN: return "LESS_THAN";
        case Op::LESS_THAN_OR_EQUAL_TO: return "LESS_THAN_OR_EQUAL_TO";
        case Op::GREATER_THAN: return "GREATER_THAN";
        case Op::GREATER_THAN_OR_EQUAL_TO: return "GREATER_THAN_OR_EQUAL_TO";
        case Op::SUBTRACT: return "SUBTRACT";
        case Op::ADD: return "ADD";
        case Op::DIVIDE: return "DIVIDE";
        case Op::MULTIPLY: return "MULTIPLY";
        case Op::NEGATE: return "NEGATE";
        case Op::NOT: return "NOT";
        case Op::LITERAL: return "LITERAL";
        }
    }

    Expr() = default;

    ~Expr() {
        for (Expr* child : children) {
            if (child) delete child;
        }
    }

    static Expr* from_literal_token(const Token& token) {
        Literal::Literal literal;
        switch (token.type) {
        case Token::Type::TRUE:
            literal = true;
            break;
        case Token::Type::FALSE:
            literal = false;
            break;
        case Token::Type::NIL:
            literal = nullptr;
            break;
        case Token::Type::NUMBER:
            literal = std::get<double>(token.value);
            break;
        case Token::Type::STRING:
            literal = std::get<std::string>(token.value);
            break;
        default:
            return nullptr;
        }

        Expr* expr = new Expr();
        expr->op = Op::LITERAL;
        expr->literal = literal;
        return expr;
    }

    static Expr* unary_op(const Token& token, Expr* operand) {
        Op op;

        switch (token.type) {
        case Token::Type::MINUS: op = Op::NEGATE; break;
        case Token::Type::BANG:  op = Op::NOT;    break;
        default:
            return nullptr;
        }

        Expr* expr = new Expr();
        expr->op = op;
        expr->children.emplace_back(operand);
        return expr;
    }

    static Expr* binary_op(const Token& token, Expr* lhs, Expr* rhs) {
        Op op;

        switch (token.type) {
        case Token::Type::SLASH: op = Op::DIVIDE; break;
        case Token::Type::STAR:  op = Op::MULTIPLY; break;
        case Token::Type::MINUS: op = Op::SUBTRACT; break;
        case Token::Type::PLUS:  op = Op::ADD; break;

        case Token::Type::GREATER:       op = Op::GREATER_THAN; break;
        case Token::Type::GREATER_EQUAL: op = Op::GREATER_THAN_OR_EQUAL_TO; break;
        case Token::Type::LESS:          op = Op::LESS_THAN; break;
        case Token::Type::LESS_EQUAL:    op = Op::LESS_THAN_OR_EQUAL_TO; break;

        case Token::Type::EQUAL_EQUAL: op = Op::IS_EQUAL; break;
        case Token::Type::BANG_EQUAL:  op = Op::IS_NOT_EQUAL; break;

        default: return nullptr;
        }

        Expr* expr = new Expr();
        expr->op = op;
        expr->children.emplace_back(lhs);
        expr->children.emplace_back(rhs);
        return expr;
    }

    std::string str() const {
        if (op == Op::LITERAL) {
            return Literal::to_string(literal);
        }

        std::ostringstream oss;
        oss << "(";
        oss << op_to_str(op);
        for (Expr* child : children) {
            oss << " " << (child ? child->str() : "?");
        }
        oss << ")";
        return oss.str();
    }
};

class Parser {
public:
    struct Error {
        int line;
        // @pet
        enum class Type {
            UNKNOWN,
            EXPRESSION_EXPECTED,
            UNCLOSED_PAREN,
            PRIMARY_EXPECTED,
            LITERAL_TOKEN_EXPECTED,
            UNEXPECTED_TOKEN,
            UNARY_OPERAND_EXPECTED,
            INVALID_UNARY_OPERAND,
            INVALID_UNARY_OPERATOR,
            BINARY_LHS_EXPECTED,
            INVALID_BINARY_OPERATOR,
            BINARY_RHS_EXPECTED,
            INVALID_BINARY_OPERAND,
        } type;

        std::string str() const {
            std::ostringstream oss;

            oss << "ERROR [Parser]: ";
            switch (type) {
            case Type::EXPRESSION_EXPECTED:
                oss << "[EXPRESSION_EXPECTED] A valid expression is expected ";
                break;
            case Type::UNCLOSED_PAREN:
                oss << "[UNCLOSED_PAREN] Unclosed parentheses ";
                break;
            case Type::PRIMARY_EXPECTED:
                oss << "[PRIMARY_EXPECTED] A number, string, boolean, nil, or (parenthesized expression) is expected ";
                break;
            case Type::LITERAL_TOKEN_EXPECTED:
                oss << "[LITERAL_TOKEN_EXPECTED] A number, string, boolean, or nil is expected ";
                break;
            case Type::UNEXPECTED_TOKEN:
                oss << "[UNEXPECTED_TOKEN] An unexpected character is present ";
                break;
            case Type::UNARY_OPERAND_EXPECTED:
                oss << "[UNARY_OPERAND_EXPECTED] A number, string, boolean, nil, or (parenthesized expression) is expected after the unary operation ( \"-\", \"!\" ) ";
                break;
            case Type::INVALID_UNARY_OPERAND:
                oss << "[INVALID_UNARY_OPERAND] Invalid unary operand ";
                break;
            case Type::INVALID_UNARY_OPERATOR:
                oss << "[INVALID_UNARY_OPERAND] Invalid unary operator, expected \"-\" or \"!\" ";
                break;
            case Type::BINARY_LHS_EXPECTED:
                oss << "[BINARY_LHS_EXPECTED] A number, string, boolean, nil, or (parenthesized expression) is expected before the binary operator ";
                break;
            case Type::INVALID_BINARY_OPERATOR:
                oss << "[INVALID_BINARY_OPERATOR] Invalid binary operator, expected arithmetic or comparison operator ";
                break;
            case Type::BINARY_RHS_EXPECTED:
                oss << "[BINARY_RHS_EXPECTED] A number, string, boolean, nil, or (parenthesized expression) is expected after the binary operator ";
                break;
            case Type::INVALID_BINARY_OPERAND:
                oss << "[INVALID_UNARY_OPERAND] Invalid binary operand ";
                break;
            default:
                oss << "[UNKNOWN] Unknown error ";
                break;
            }
            oss << "on line " << line;
            return oss.str();
        }
    };
    std::vector<Error> errors;

private:
    void add_error(Error::Type error_type, const Token& token) {
        Error error;
        error.line = token.line;
        error.type = error_type;
        errors.emplace_back(error);
    }

    void add_error_with_current_token(Error::Type error_type) {
        const Token& token = (index >= tokens.size())
            ? tokens[tokens.size() - 1] // out of tokens, use last token
            : tokens[index];
        add_error(error_type, token);
    }

    inline Token* current_token() {
        return (index < tokens.size())
            ? &tokens[index]
            : nullptr;
    }

    inline void consume_token() {
        ++index;
    }

    inline void backtrack_token() {
        --index;
    }

    Expr* literal() {
        Expr* expr = Expr::from_literal_token(*current_token());
        return expr;
    }

    Expr* grouping() {
        consume_token();
        Expr* inner_expr = expression();

        consume_token();
        if (!current_token() || current_token()->type != Token::Type::RIGHT_PAREN) {
            add_error_with_current_token(Error::Type::UNCLOSED_PAREN);
            return nullptr;
        }

        return inner_expr;
    }

    Expr* primary() {
        if (!current_token()) {
            add_error_with_current_token(Error::Type::PRIMARY_EXPECTED);
            return nullptr;
        }

        if (current_token()->is_literal_type()) {
            return literal();
        }

        if (current_token()->type == Token::Type::LEFT_PAREN) {
            return grouping();
        }

        return nullptr;
    }

    Expr* unary() {
        if (!current_token()) {
            add_error_with_current_token(Error::Type::UNARY_OPERAND_EXPECTED);
            return nullptr;
        }

        switch (current_token()->type) {
        case Token::Type::MINUS:
        case Token::Type::BANG: {
            const Token& operator_token = *current_token();

            consume_token();
            Expr* operand = unary();
            if (!operand) {
                add_error_with_current_token(Error::Type::INVALID_UNARY_OPERAND);
                return nullptr;
            }

            Expr* expr = Expr::unary_op(operator_token, operand);
            if (!expr) {
                add_error_with_current_token(Error::Type::INVALID_UNARY_OPERATOR);
            }
            return expr;
        }

        default:
            return primary();
        }
    }

    Expr* binary(
        const std::vector<Token::Type>& op_token_types,
        Expr* (Parser::*constituent)()
    ) {
        Expr* lhs = (this->*constituent)();
        if (!lhs) {
            add_error_with_current_token(Error::Type::BINARY_LHS_EXPECTED);
            return nullptr;
        }

        Expr* parent_expr = lhs;
        Expr* rhs = nullptr;

        consume_token();
        while (current_token()) {
            // if we are chaining operations together
            // use the previously parsed Expr (parent_expr)
            // as the lhs and determine new rhs
            if (rhs) {
                lhs = parent_expr;
                rhs = nullptr;
            }

            // determine if this is an operator token
            bool is_op_token = false;
            for (Token::Type token_type : op_token_types) {
                if (current_token()->type == token_type) {
                    is_op_token = true;
                    break;
                }
            }
            if (!is_op_token) {
                // this is not necessarily an invalid operator,
                // it could be an operator of lower precedence.
                //
                // return control to caller and let them handle it
                backtrack_token();
                return lhs;
            }

            const Token& operator_token = *current_token();

            // process rhs
            consume_token();
            rhs = (this->*constituent)();
            if (!rhs) {
                add_error_with_current_token(Error::Type::BINARY_RHS_EXPECTED);
                return nullptr;
            }

            parent_expr = Expr::binary_op(operator_token, lhs, rhs);
            if (!parent_expr) {
                add_error_with_current_token(Error::Type::INVALID_BINARY_OPERATOR);
                return nullptr;
            }

            consume_token();
        }

        return parent_expr;
    }

    Expr* multiplicative() {
        return binary(
            {Token::Type::SLASH, Token::Type::STAR},
            &Parser::unary
        );
    }

    Expr* additive() {
        return binary(
            {Token::Type::MINUS, Token::Type::PLUS},
            &Parser::multiplicative
        );
    }

    Expr* comparison() {
        return binary(
            {
                Token::Type::LESS,    Token::Type::LESS_EQUAL,
                Token::Type::GREATER, Token::Type::GREATER_EQUAL
            },
            &Parser::additive
        );
    }

    Expr* equality() {
        return binary(
            {Token::Type::EQUAL_EQUAL, Token::Type::BANG_EQUAL},
            &Parser::comparison
        );
    }

    Expr* expression() {
        Expr* expr = equality();
        return expr;
    }

    void parse() {
        if (tokens.size() == 0) {
            return;
        }

        index = 0;
        root_expr = expression();

        consume_token();
        if (current_token()) {
            add_error(Error::Type::UNEXPECTED_TOKEN, *current_token());
        }
    }

public:
    Expr* root_expr = nullptr;
    std::vector<Token> tokens;
    size_t index = 0;

    Parser(std::vector<Token>&& tokens)
        : tokens(tokens)
    {
        parse();
    }

    ~Parser() {
        if (root_expr) {
            delete root_expr;
        }
    }

    bool valid() const {
        return errors.empty();
    }
};


// =====
// basic lox interface
void show_tokens(const std::string& code) {
    Scanner scanner(code);

    for (const auto& token : scanner.tokens) {
        std::cout << token.str() << std::endl;
    }

    if (!scanner.valid()) {
        report_errors(scanner.errors, scanner.lines);
    }
}

void show_ast(const std::string& code) {
    Scanner scanner(code);
    if (!scanner.valid()) {
        report_errors(scanner.errors, scanner.lines);
        return;
    }

    Parser parser(std::move(scanner.tokens));
    if (!parser.valid()) {
        report_errors(parser.errors, scanner.lines);
        return;
    }
    if (parser.root_expr) {
        std::cout << parser.root_expr->str() << std::endl;
    }
}

void run(const std::string& code) {
    show_ast(code);
}

// =====
// cmd tool run modes
void file_show_tokens(const char* filename) {
    std::string code = str_from_file(filename);
    show_tokens(code);
}

void file_show_ast(const char* filename) {
    std::string code = str_from_file(filename);
    show_ast(code);
}

void file_run(const char* filename) {
    std::string code = str_from_file(filename);
    run(code);
}

void prompt_run() {
    std::string code;
    while (true) {
        std::cout << "lox> ";
        std::getline(std::cin, code);
        if (code.empty()) {
            break;
        }
        run(code);
    }
}

// =====
// main
int main(int argc, char** argv) {
    if (argc > 3) {
        std::cerr << "Usage: ./lox [script]" << std::endl;
        return EX_USAGE;
    } else if (argc == 3) {
        std::string command = argv[1];
        std::string file = argv[2];
        if (command == "tokens") {
            file_show_tokens(file.c_str());
        } else if (command == "ast") {
            file_show_ast(file.c_str());
        } else if (command == "exec") {
            run(argv[2]);
        } else {
            file_run(file.c_str());
        }
    } else if (argc == 2) {
        file_run(argv[1]);
    } else {
        prompt_run();
    }

    return 0;
}
