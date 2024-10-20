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
// lox
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
        case Type::THIS: return "THIS";
        case Type::TRUE: return "TRUE";
        case Type::VAR: return "VAR";
        case Type::WHILE: return "WHILE";
        case Type::EOF_TOKEN: return "EOF_TOKEN";
        default: return std::to_string((int)type);
        }
    }

    int line;
    Type type;
    std::variant<nullptr_t, double, std::string> value;

    Token(int line, Type type)
        : type(type)
        , value(nullptr)
    {
    }

    Token(int line, Type type, double value)
        : type(type)
        , value(value)
    {
    }

    Token(int line, Type type, const std::string& value)
        : type(type)
        , value(value)
    {
    }

    std::string str() const {
        std::ostringstream oss;
        oss << "<Token"
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
                oss << "Unterminated string ";
                break;
            default:
                oss << "Unknown ";
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
    size_t start_line = 0;

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

    void output_errors() const {
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
        }
    }
};

class Ast {
public:
    Ast(std::vector<Token>&& tokens) {
    }

    std::string str() const {
        return "AST: ";
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
        scanner.output_errors();
    }
}

void show_ast(const std::string& code) {
    Scanner scanner(code);

    if (!scanner.valid()) {
        scanner.output_errors();
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
