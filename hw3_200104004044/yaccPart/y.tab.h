/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

#ifndef YY_YY_Y_TAB_H_INCLUDED
# define YY_YY_Y_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    VALUEF = 258,                  /* VALUEF  */
    IDENTIFIER = 259,              /* IDENTIFIER  */
    KW_AND = 260,                  /* KW_AND  */
    KW_OR = 261,                   /* KW_OR  */
    KW_NOT = 262,                  /* KW_NOT  */
    KW_EQUAL = 263,                /* KW_EQUAL  */
    KW_LESS = 264,                 /* KW_LESS  */
    KW_NIL = 265,                  /* KW_NIL  */
    KW_LIST = 266,                 /* KW_LIST  */
    KW_APPEND = 267,               /* KW_APPEND  */
    KW_CONCAT = 268,               /* KW_CONCAT  */
    KW_SET = 269,                  /* KW_SET  */
    KW_DEF = 270,                  /* KW_DEF  */
    KW_FOR = 271,                  /* KW_FOR  */
    KW_IF = 272,                   /* KW_IF  */
    KW_EXIT = 273,                 /* KW_EXIT  */
    KW_LOAD = 274,                 /* KW_LOAD  */
    KW_DISPLAY = 275,              /* KW_DISPLAY  */
    KW_TRUE = 276,                 /* KW_TRUE  */
    KW_FALSE = 277,                /* KW_FALSE  */
    OP_PLUS = 278,                 /* OP_PLUS  */
    OP_MINUS = 279,                /* OP_MINUS  */
    OP_DIV = 280,                  /* OP_DIV  */
    OP_MULT = 281,                 /* OP_MULT  */
    OP_OP = 282,                   /* OP_OP  */
    OP_CP = 283,                   /* OP_CP  */
    OP_COMMA = 284,                /* OP_COMMA  */
    COMMENT = 285                  /* COMMENT  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif
/* Token kinds.  */
#define YYEMPTY -2
#define YYEOF 0
#define YYerror 256
#define YYUNDEF 257
#define VALUEF 258
#define IDENTIFIER 259
#define KW_AND 260
#define KW_OR 261
#define KW_NOT 262
#define KW_EQUAL 263
#define KW_LESS 264
#define KW_NIL 265
#define KW_LIST 266
#define KW_APPEND 267
#define KW_CONCAT 268
#define KW_SET 269
#define KW_DEF 270
#define KW_FOR 271
#define KW_IF 272
#define KW_EXIT 273
#define KW_LOAD 274
#define KW_DISPLAY 275
#define KW_TRUE 276
#define KW_FALSE 277
#define OP_PLUS 278
#define OP_MINUS 279
#define OP_DIV 280
#define OP_MULT 281
#define OP_OP 282
#define OP_CP 283
#define OP_COMMA 284
#define COMMENT 285

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 17 "gpp_interpreter.y"

    char* str;
    char* valuef;
    char* identifier;
    struct ast_node* node;
    struct function_def* function;

#line 135 "y.tab.h"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;


int yyparse (void);


#endif /* !YY_YY_Y_TAB_H_INCLUDED  */
