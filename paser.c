#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

//Tokens
typedef enum {
  EOF_TOKEN,
  INT,
  PRINT,
  IDENTIFIER,
  NUMBER,
  SEMICOLON,
  LPAREN,
  RPAREN,
  ASSIGN,
  PLUS,
  MINUS,
  MUL,
  DIV,
  INVALID
} TokenType;

typedef struct {
  TokenType type;       // kind of token
  char text[64];        // text
  int number;           
  int line;            
} Token;

//Lexer 
typedef struct {
  const char *source;   
  size_t index;         
  int line;             
} Lexer;

//Symbol table
typedef struct {
  char name[64];
  int value;
  int hasValue;         // 0 = declared only, 1 = assigned
} Variable;

static Variable variables[256];
static int varCount = 0;

static void die(int line, const char *msg) {
  fprintf(stderr, "Error at line %d: %s\n", line, msg);
  exit(1);
}

static int findVariable(const char *name) {
  for (int i = 0; i < varCount; i++) {
    if (strcmp(variables[i].name, name) == 0) return i;
  }
  return -1;
}
//add new variable 
static void declareVariable(int line, const char *name) {
  if (findVariable(name) != -1) die(line, "Semantic error: variable redeclared");
  if (varCount >= 256) die(line, "Too many variables");

  strncpy(variables[varCount].name, name, sizeof(variables[varCount].name) - 1);
  variables[varCount].name[sizeof(variables[varCount].name) - 1] = '\0';
  variables[varCount].value = 0; //add not value before set value
  variables[varCount].hasValue = 0;
  varCount++;
}

//LEXER HELPERS
static char peekChar(Lexer *L) {
  return L->source[L->index];
}

static char takeChar(Lexer *L) {
  char c = L->source[L->index];
  if (c == '\n') L->line++;
  if (c != '\0') L->index++;
  return c;
}

static void skipSpaces(Lexer *L) {
  while (isspace((unsigned char)peekChar(L))) {
    takeChar(L);
  }
}

static Token makeToken(TokenType type, int line) {
  Token t;
  t.type = type;
  t.text[0] = '\0';
  t.number = 0;
  t.line = line;
  return t;
}

static Token nextToken(Lexer *L) {
  skipSpaces(L);
  int line = L->line;
  char c = peekChar(L);

  if (c == '\0') return makeToken(EOF_TOKEN, line);

  /* Single-character tokens */
  if (c == ';') { takeChar(L); return makeToken(SEMICOLON, line); }
  if (c == '(') { takeChar(L); return makeToken(LPAREN, line); }
  if (c == ')') { takeChar(L); return makeToken(RPAREN, line); }
  if (c == '=') { takeChar(L); return makeToken(ASSIGN, line); }
  if (c == '+') { takeChar(L); return makeToken(PLUS, line); }
  if (c == '-') { takeChar(L); return makeToken(MINUS, line); }
  if (c == '*') { takeChar(L); return makeToken(MUL, line); }
  if (c == '/') { takeChar(L); return makeToken(DIV, line); }

  /* Number token */
  if (isdigit((unsigned char)c)) {
    Token t = makeToken(NUMBER, line);
    int value = 0;
    while (isdigit((unsigned char)peekChar(L))) {
      value = value * 10 + (takeChar(L) - '0');
    }
    t.number = value;
    return t;
  }

  /* Identifier or keyword token */
  if (isalpha((unsigned char)c) || c == '_') {
    Token t = makeToken(IDENTIFIER, line);
    size_t start = L->index;

    while (isalnum((unsigned char)peekChar(L)) || peekChar(L) == '_') {
      takeChar(L);
    }

    size_t len = L->index - start;
    if (len >= sizeof(t.text)) len = sizeof(t.text) - 1;
    strncpy(t.text, L->source + start, len);
    t.text[len] = '\0';

    if (strcmp(t.text, "int") == 0) t.type = INT;
    else if (strcmp(t.text, "print") == 0) t.type = PRINT;

    return t;
  }

  /* Unknown character */
  takeChar(L);
  return makeToken(INVALID, line);
}

//PARSER 
typedef struct {
  Lexer lexer;
  Token current;
} Parser;

static void expect(Parser *P, TokenType type, const char *msg) {
  if (P->current.type != type) die(P->current.line, msg);
  P->current = nextToken(&P->lexer);
}

/* Forward declaration */
static int parseExpression(Parser *P);

static int parseFactor(Parser *P) {
  Token t = P->current;

  if (t.type == NUMBER) {
    int v = t.number;
    expect(P, NUMBER, "Expected number");
    return v;
  }

  if (t.type == IDENTIFIER) {
    int idx = findVariable(t.text);
    if (idx == -1) die(t.line, "Semantic error: variable used before declaration");
    if (!variables[idx].hasValue) die(t.line, "Semantic error: variable used before assignment");

    int v = variables[idx].value;
    expect(P, IDENTIFIER, "Expected identifier");
    return v;
  }

  /* Unary minus, e.g. -5 or -(x + 1) */
  if (t.type == MINUS) {
    expect(P, MINUS, "Expected '-'");
    return -parseFactor(P);
  }

  if (t.type == LPAREN) {
    expect(P, LPAREN, "Expected '('");
    int v = parseExpression(P);
    expect(P, RPAREN, "Expected ')'");
    return v;
  }

  die(t.line, "Expected factor (number, identifier, or (expression))");
  return 0;
}

static int parseTerm(Parser *P) {
  int left = parseFactor(P);

  while (P->current.type == MUL || P->current.type == DIV) {
    TokenType op = P->current.type;
    int opLine = P->current.line;
    expect(P, op, "Expected * or /");
    int right = parseFactor(P);

    if (op == MUL) left = left * right;
    else {
      if (right == 0) die(opLine, "Semantic error: division by zero");
      left = left / right;
    }
  }

  return left;
}

static int parseExpression(Parser *P) {
  int left = parseTerm(P);

  while (P->current.type == PLUS || P->current.type == MINUS) {
    TokenType op = P->current.type;
    expect(P, op, "Expected + or -");
    int right = parseTerm(P);

    if (op == PLUS) left = left + right;
    else left = left - right;
  }

  return left;
}

static void parseDeclaration(Parser *P) {
  int line = P->current.line;
  expect(P, INT, "Expected 'int'");

  if (P->current.type != IDENTIFIER) die(P->current.line, "Expected variable name after int");
  char varName[64];
  strncpy(varName, P->current.text, sizeof(varName) - 1);
  varName[sizeof(varName) - 1] = '\0';

  declareVariable(line, varName);
  expect(P, IDENTIFIER, "Expected identifier");

  int idx = findVariable(varName);

  if (P->current.type == ASSIGN) {
    expect(P, ASSIGN, "Expected '='");
    int value = parseExpression(P);
    variables[idx].value = value;
    variables[idx].hasValue = 1;
  }

  expect(P, SEMICOLON, "Expected ';' after declaration");
}

static void parseAssignment(Parser *P) {
  Token id = P->current;
  if (id.type != IDENTIFIER) die(id.line, "Expected variable name");

  int idx = findVariable(id.text);
  if (idx == -1) die(id.line, "Semantic error: assignment to undeclared variable");

  expect(P, IDENTIFIER, "Expected identifier");
  expect(P, ASSIGN, "Expected '=' in assignment");

  int value = parseExpression(P);
  variables[idx].value = value;
  variables[idx].hasValue = 1;

  expect(P, SEMICOLON, "Expected ';' after assignment");
}

static void parsePrint(Parser *P) {
  expect(P, PRINT, "Expected 'print'");
  expect(P, LPAREN, "Expected '(' after print");

  int value = parseExpression(P);

  expect(P, RPAREN, "Expected ')'");
  expect(P, SEMICOLON, "Expected ';' after print");

  printf("%d\n", value);
}

static void parseStatement(Parser *P) {
  if (P->current.type == INT) {
    parseDeclaration(P);
  } else if (P->current.type == PRINT) {
    parsePrint(P);
  } else if (P->current.type == IDENTIFIER) {
    parseAssignment(P);
  } else if (P->current.type == INVALID) {
    die(P->current.line, "Invalid token");
  } else {
    die(P->current.line, "Unexpected token at start of statement");
  }
}

static void parseProgram(Parser *P) {
  while (P->current.type != EOF_TOKEN) {
    parseStatement(P);
  }
}

//FILE READ
static char *readFile(const char *path) {
  FILE *f = fopen(path, "rb");
  if (!f) return NULL;

  if (fseek(f, 0, SEEK_END) != 0) { fclose(f); return NULL; }
  long size = ftell(f);
  if (size < 0) { fclose(f); return NULL; }
  rewind(f);

  char *buffer = (char *)malloc((size_t)size + 1);
  if (!buffer) { fclose(f); return NULL; }

  size_t got = fread(buffer, 1, (size_t)size, f);
  buffer[got] = '\0';
  fclose(f);
  return buffer;
}

 
int main(int argc, char **argv) {
  if (argc < 2) {
    fprintf(stderr, "Usage: %s <inputfile>\n", argv[0]);
    return 1;
  }

  char *programText = readFile(argv[1]);
  if (!programText) {
    fprintf(stderr, "Could not read file: %s\n", argv[1]);
    return 1;
  }

  Parser P;
  P.lexer.source = programText;
  P.lexer.index = 0;
  P.lexer.line = 1;
  P.current = nextToken(&P.lexer);

  parseProgram(&P);

  free(programText);
  return 0;
}
