#include <cctype>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <sysexits.h>
#include <variant>
#include <vector>

// =====
// utils
std::string str_from_file(char* filename) {
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
// lox
class Token {
public:
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
        IDENTIFIER, STRING, NUMBER,

        // keywords
        AND,
        CLASS,
        ELSE,
        FALSE,
        FUN,
        FOR,
        IF,
        NIL,
        OR,
        PRINT,
        RETURN,
        SUPER,
        THIS,
        TRUE,
        VAR,
        WHILE,

        //
        EOF_TOKEN
    };

    Type type;
    std::variant<nullptr_t, double, std::string> value;


    Token(Type type)
        : type(type)
        , value(nullptr)
    {
    }

    Token(Type type, double value)
        : type(type)
        , value(value)
    {
    }

    Token(Type type, const std::string& value)
        : type(type)
        , value(value)
    {
    }


    std::string str() const {
        std::ostringstream oss;
        oss << "<Token"
            << " type=" << (int)type;
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
    enum class Status {
        UNKNOWN,
        SUCCESS,
        UNTERMINATED_STRING,
    };

    struct Output {
        std::vector<Token> tokens;
        Status status;
    };

private:
    enum class State {
        DEFAULT,
        COMMENT,
        STRING,
        NUMBER,
        KEYWORD_OR_IDENTIFIER
    } state = State::DEFAULT;

    std::vector<Token> tokens;
    const std::string& code;
    size_t index = 0;

    void add_token(Token::Type token_type) {
        tokens.emplace_back(Token(token_type));
    }

    void add_token(Token::Type token_type, double value) {
        tokens.emplace_back(Token(token_type, value));
    }

    void add_token(Token::Type token_type, const std::string& value) {
        tokens.emplace_back(Token(token_type, value));
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
            } else if (std::isalpha(code[index])) {
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
        return std::isalpha(c) || std::isdigit(c);
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


public:
    Scanner(const std::string& code)
        : code(code) {
    }

    Output scan_tokens() {
        tokens = {};
        index = 0;

        while (index < code.size()) {
            advance();
        }

        Output output;

        output.tokens = std::move(tokens);

        output.status = Status::SUCCESS;
        if (state == State::STRING) output.status = Status::UNTERMINATED_STRING;

        return output;
    }
};

std::vector<Token> scan_tokens(const std::string& code) {
    Scanner::Output scanner_output = Scanner(code).scan_tokens();

    if (scanner_output.status == Scanner::Status::UNTERMINATED_STRING) {
        std::cerr << "ERROR: Unterminated string" << std::endl;
    }

    return scanner_output.tokens;
}

void run(const std::string& code) {
    std::vector<Token> tokens = scan_tokens(code);
    for (const Token& token : tokens) {
        std::cout << token.str() << std::endl;
    }
}

// =====
// IO
void run_file(char* filename) {
    std::string code = str_from_file(filename);
    run(code);
}

void run_prompt() {
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
    if (argc > 2) {
        std::cerr << "Usage: ./lox [script]" << std::endl;
        return EX_USAGE;
    } else if (argc == 2) {
        run_file(argv[1]);
    } else {
        run_prompt();
    }

    return 0;
}
