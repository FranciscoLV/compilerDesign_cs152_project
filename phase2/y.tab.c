/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.4"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 1 "mini_l.y" /* yacc.c:339  */

 #include <stdio.h>
 #include <stdlib.h>
 void yyerror(const char *msg);
 int yylex();
 extern int currentLine;
 extern int currentPosition;
 FILE * yyin;

#line 76 "y.tab.c" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 1
#endif

/* In a future release of Bison, this section will be replaced
   by #include "y.tab.h".  */
#ifndef YY_YY_Y_TAB_H_INCLUDED
# define YY_YY_Y_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    FUNCTION = 258,
    BEGIN_PARAMS = 259,
    END_PARAMS = 260,
    BEGIN_LOCALS = 261,
    END_LOCALS = 262,
    BEGIN_BODY = 263,
    END_BODY = 264,
    INTEGER = 265,
    ARRAY = 266,
    OF = 267,
    IF = 268,
    THEN = 269,
    ENDIF = 270,
    ELSE = 271,
    WHILE = 272,
    DO = 273,
    BEGINLOOP = 274,
    ENDLOOP = 275,
    CONTINUE = 276,
    READ = 277,
    WRITE = 278,
    AND = 279,
    OR = 280,
    NOT = 281,
    TRUE = 282,
    FALSE = 283,
    RETURN = 284,
    SUB = 285,
    ADD = 286,
    MULT = 287,
    DIV = 288,
    MOD = 289,
    EQ = 290,
    NEQ = 291,
    LT = 292,
    GT = 293,
    LTE = 294,
    GTE = 295,
    NUMBER = 296,
    IDENT = 297,
    SEMICOLON = 298,
    COLON = 299,
    COMMA = 300,
    L_PAREN = 301,
    R_PAREN = 302,
    L_BRACKET = 303,
    R_BRACKET = 304,
    ASSIGN = 305
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 11 "mini_l.y" /* yacc.c:355  */

  double  nval;
  char  *idval;

#line 172 "y.tab.c" /* yacc.c:355  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */

/* Copy the second part of user declarations.  */

#line 189 "y.tab.c" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  5
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   166

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  51
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  72
/* YYNRULES -- Number of rules.  */
#define YYNRULES  109
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  181

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   305

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint8 yyrline[] =
{
       0,    47,    47,    47,    49,    52,    52,    54,    55,    57,
      58,    60,    61,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    73,    74,    76,    78,    78,    80,    82,    82,
      84,    85,    87,    88,    89,    90,    92,    93,    94,    95,
      96,    97,    99,   101,   101,   102,   104,   106,   106,   107,
     108,   110,   111,   112,   114,   114,   115,   117,   118,   119,
     121,   122,   125,   127,   129,   131,   133,   135,   137,   139,
     141,   143,   145,   147,   149,   151,   153,   155,   157,   159,
     161,   163,   165,   167,   169,   171,   173,   175,   177,   179,
     181,   183,   185,   187,   189,   191,   193,   195,   197,   199,
     201,   203,   205,   207,   209,   211,   213,   215,   217,   219
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 1
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "FUNCTION", "BEGIN_PARAMS", "END_PARAMS",
  "BEGIN_LOCALS", "END_LOCALS", "BEGIN_BODY", "END_BODY", "INTEGER",
  "ARRAY", "OF", "IF", "THEN", "ENDIF", "ELSE", "WHILE", "DO", "BEGINLOOP",
  "ENDLOOP", "CONTINUE", "READ", "WRITE", "AND", "OR", "NOT", "TRUE",
  "FALSE", "RETURN", "SUB", "ADD", "MULT", "DIV", "MOD", "EQ", "NEQ", "LT",
  "GT", "LTE", "GTE", "NUMBER", "IDENT", "SEMICOLON", "COLON", "COMMA",
  "L_PAREN", "R_PAREN", "L_BRACKET", "R_BRACKET", "ASSIGN", "$accept",
  "program", "funct_start", "ds_loop", "ss_loop", "declaration", "i_loop",
  "statement", "var_loop", "bool_expr", "ro_loop", "relation_and_expr",
  "ra_loop", "relation_expr", "n_loop", "comp", "expression", "as_loop",
  "multiplicative_expr", "mdm_loop", "term", "ec_loop", "s_loop", "var",
  "function", "identifier", "semicolon", "beginparams", "endparams",
  "beginlocals", "endlocals", "beginbody", "endbody", "comma", "colon",
  "integer", "array", "l_bracket", "number", "r_bracket", "of", "assign",
  "if", "then", "else", "endif", "while", "beginloop", "endloop", "do",
  "read", "write", "continue", "return", "or", "and", "not", "true",
  "false", "l_paren", "r_paren", "eq", "neq", "lt", "gt", "lte", "gte",
  "add", "sub", "mult", "div", "mod", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305
};
# endif

#define YYPACT_NINF -88

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-88)))

#define YYTABLE_NINF -1

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
       6,   -88,    13,     6,   -25,   -88,   -88,   -88,   -23,   -88,
      35,   -88,   -25,    36,   -23,    37,     5,   -88,    73,   -25,
     -88,    91,   -88,   -25,   -88,   -25,   -88,   -88,   -88,   -88,
      41,   -88,    76,   -88,    52,   -88,    89,   -88,    64,   -88,
      69,   -88,   110,   -88,   -88,   -88,   -88,   -88,   -88,   -88,
     115,   -23,    53,    41,     1,     1,   111,   -25,   -25,   -88,
      10,   -88,   121,   -88,   -88,    69,   -88,    10,    10,   -88,
     -88,   -88,   -88,   -88,   118,   108,   112,   -88,   103,    77,
      -8,   -88,   -88,   -32,   -88,    32,   -88,   -88,     1,    54,
     111,   -88,    69,   -88,     5,   -88,   -88,    10,   -88,   -88,
     -88,    64,   -88,    69,   -88,   -88,     1,   -88,   -88,     1,
     -88,   -88,   -88,   -88,   -88,   -88,    10,   -88,   -88,   -88,
     -88,   -88,   -88,   -88,   -88,    10,    10,   -88,   -88,   -88,
     -88,    10,    10,    10,    10,   -88,    87,    79,   -88,    69,
     124,   -25,    87,   -88,    94,   108,   112,   -88,    77,    77,
      -8,    -8,    -8,     5,    87,   -88,   -88,   -88,   124,   -88,
     128,   -88,   -88,   -88,    69,   -88,   -88,   -88,   -88,   -88,
     -88,   -88,   -88,    10,   -88,   -88,     1,   131,   -88,   -88,
     -88
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       2,    62,     0,     2,     0,     1,     3,    63,     0,    64,
       0,    65,     5,     0,     0,     0,    12,    66,     0,     5,
      72,     0,    71,     0,    67,     5,     6,    73,    74,     9,
       0,    11,     0,    75,     0,    68,     0,    76,     0,    69,
       0,    77,     0,    80,    84,    87,    90,    88,    89,    91,
       0,     0,     0,    61,     0,     0,     0,     0,     0,    20,
       0,    78,     0,    70,     4,     7,    79,     0,     0,    94,
      95,    96,   106,    97,     0,    25,    28,    31,     0,    43,
      47,    53,    59,    61,    58,     0,    33,    34,     0,     0,
       0,    85,     0,    18,    23,    19,    21,     0,    10,     8,
      13,     0,    81,     0,    92,    24,     0,    93,    27,     0,
      99,   100,   101,   102,   103,   104,     0,    36,    37,    38,
      39,    40,    41,   105,    42,     0,     0,   107,   108,   109,
      46,     0,     0,     0,    54,    30,     0,     0,    52,     0,
       0,     0,     0,    60,     0,    25,    28,    32,    43,    43,
      47,    47,    47,    56,     0,    98,    35,    57,     0,    86,
       0,    22,    83,    82,     0,    14,    26,    29,    44,    45,
      48,    49,    50,    54,    51,    16,     0,     0,    55,    17,
      15
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
     -88,   144,   -88,    -7,   -59,   -88,   125,   -88,   -57,   -53,
       4,    44,     7,    43,    70,   -88,   -22,   -28,     2,   -80,
     -68,   -19,    67,   -35,   -88,    -4,    -3,   -88,   -88,   -88,
     -88,   -88,   -88,   -84,   -88,    95,   -88,   129,   127,    57,
     -88,   -88,   -88,   -88,   -88,   -15,     3,    74,     8,   -88,
     -88,   -88,   -88,   -88,   -88,   -88,   -88,   -88,   -88,   -51,
     -87,   -88,   -88,   -88,   -88,   -88,   -88,   -88,   -72,   -88,
     -88,   -88
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     2,     3,    13,    50,    14,    15,    51,    93,    74,
     105,    75,   108,    76,    77,   116,    78,   124,    79,   130,
      80,   154,    81,    82,     4,    83,    10,    12,    18,    25,
      36,    40,    64,    23,    21,    29,    30,    68,    84,    42,
      62,    67,    54,   103,   164,   165,    55,    92,   160,    56,
      57,    58,    59,    60,   106,   109,    85,    86,    87,    97,
     157,   117,   118,   119,   120,   121,   122,   125,    89,   131,
     132,   133
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_uint8 yytable[] =
{
       8,    95,    90,    88,    88,    52,    99,   126,    16,     1,
     141,    19,    26,     5,    73,    16,    33,     7,    32,    16,
       9,    16,    94,    94,   127,   128,   129,    69,    70,    71,
      52,    72,   134,   140,    88,   136,    53,    88,    96,    11,
      72,    17,    37,     7,   144,   100,   101,    73,    65,   156,
      22,    37,     7,    53,    53,    88,    73,    52,    88,    70,
      71,    53,    72,   150,   151,   152,   137,   174,    52,   173,
     170,   171,   172,    37,     7,   142,   126,   126,    73,    24,
     158,    20,    43,    35,   161,    53,    44,    45,    53,    33,
      46,    47,    48,    37,   147,    37,     7,    39,    49,    53,
      73,    27,    28,    66,    52,   177,    94,    72,   123,   162,
     163,     7,   153,    41,   110,   111,   112,   113,   114,   115,
     168,   169,    61,   179,    63,    88,   155,   148,   149,    52,
      91,    27,   102,   104,   155,    53,   107,    53,   110,   111,
     112,   113,   114,   115,   159,    44,   162,     6,    31,   166,
     145,   153,   146,   167,   178,   135,   138,    98,   143,    34,
      53,    38,   180,   176,   139,     0,   175
};

static const yytype_int16 yycheck[] =
{
       4,    58,    55,    54,    55,    40,    65,    79,    12,     3,
      94,    14,    19,     0,    46,    19,    48,    42,    25,    23,
      43,    25,    57,    58,    32,    33,    34,    26,    27,    28,
      65,    30,    83,    92,    85,    88,    40,    88,    60,     4,
      30,     5,    41,    42,   103,    67,    68,    46,    51,   136,
      45,    41,    42,    57,    58,   106,    46,    92,   109,    27,
      28,    65,    30,   131,   132,   133,    88,   154,   103,   153,
     150,   151,   152,    41,    42,    97,   148,   149,    46,     6,
     139,    44,    13,     7,   141,    89,    17,    18,    92,    48,
      21,    22,    23,    41,   116,    41,    42,     8,    29,   103,
      46,    10,    11,    50,   139,   164,   141,    30,    31,    15,
      16,    42,   134,    49,    35,    36,    37,    38,    39,    40,
     148,   149,    12,   176,     9,   176,    47,   125,   126,   164,
      19,    10,    14,    25,    47,   139,    24,   141,    35,    36,
      37,    38,    39,    40,    20,    17,    15,     3,    23,   145,
     106,   173,   109,   146,   173,    85,    89,    62,   101,    30,
     164,    34,   177,   160,    90,    -1,   158
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     3,    52,    53,    75,     0,    52,    42,    76,    43,
      77,     4,    78,    54,    56,    57,    76,     5,    79,    77,
      44,    85,    45,    84,     6,    80,    54,    10,    11,    86,
      87,    57,    54,    48,    88,     7,    81,    41,    89,     8,
      82,    49,    90,    13,    17,    18,    21,    22,    23,    29,
      55,    58,    74,    76,    93,    97,   100,   101,   102,   103,
     104,    12,    91,     9,    83,    77,    50,    92,    88,    26,
      27,    28,    30,    46,    60,    62,    64,    65,    67,    69,
      71,    73,    74,    76,    89,   107,   108,   109,   110,   119,
      60,    19,    98,    59,    74,    59,    67,   110,    86,    55,
      67,    67,    14,    94,    25,    61,   105,    24,    63,   106,
      35,    36,    37,    38,    39,    40,    66,   112,   113,   114,
     115,   116,   117,    31,    68,   118,   119,    32,    33,    34,
      70,   120,   121,   122,   110,    65,    60,    67,    73,    98,
      55,    84,    67,    90,    55,    62,    64,    67,    69,    69,
      71,    71,    71,    67,    72,    47,   111,   111,    55,    20,
      99,    59,    15,    16,    95,    96,    61,    63,    68,    68,
      70,    70,    70,    84,   111,    99,    97,    55,    72,    60,
      96
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    51,    52,    52,    53,    54,    54,    55,    55,    56,
      56,    57,    57,    58,    58,    58,    58,    58,    58,    58,
      58,    58,    59,    59,    60,    61,    61,    62,    63,    63,
      64,    64,    65,    65,    65,    65,    66,    66,    66,    66,
      66,    66,    67,    68,    68,    68,    69,    70,    70,    70,
      70,    71,    71,    71,    72,    72,    72,    73,    73,    73,
      74,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    95,    96,    97,    98,    99,   100,   101,   102,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     2,    12,     0,     3,     2,     3,     3,
       8,     3,     1,     3,     5,     7,     5,     6,     2,     2,
       1,     2,     3,     1,     2,     0,     3,     2,     0,     3,
       2,     1,     3,     1,     1,     3,     1,     1,     1,     1,
       1,     1,     2,     0,     3,     3,     2,     0,     3,     3,
       3,     4,     2,     1,     0,     3,     1,     3,     1,     1,
       4,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yystacksize);

        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 3:
#line 47 "mini_l.y" /* yacc.c:1646  */
    {printf("program -> funct_start program");}
#line 1410 "y.tab.c" /* yacc.c:1646  */
    break;

  case 4:
#line 50 "mini_l.y" /* yacc.c:1646  */
    {printf("funct_start -> function identifier semicolon beginparams ds_loop endparams beginlocals ds_loop endlocals beginbody ss_loop endbody\n");}
#line 1416 "y.tab.c" /* yacc.c:1646  */
    break;

  case 6:
#line 52 "mini_l.y" /* yacc.c:1646  */
    {printf("ds_loop -> declaration semicolon ds_loop\n");}
#line 1422 "y.tab.c" /* yacc.c:1646  */
    break;

  case 8:
#line 55 "mini_l.y" /* yacc.c:1646  */
    {printf("ss_loop -> statement semicolon ss_loop\n");}
#line 1428 "y.tab.c" /* yacc.c:1646  */
    break;

  case 9:
#line 57 "mini_l.y" /* yacc.c:1646  */
    {printf("declaration -> i_loop colon integer\n");}
#line 1434 "y.tab.c" /* yacc.c:1646  */
    break;

  case 10:
#line 58 "mini_l.y" /* yacc.c:1646  */
    {printf("declaration -> i_loop colon array l_bracket r_bracket of integer\n");}
#line 1440 "y.tab.c" /* yacc.c:1646  */
    break;

  case 11:
#line 60 "mini_l.y" /* yacc.c:1646  */
    {printf("i_loop -> identifier comma i_loop\n");}
#line 1446 "y.tab.c" /* yacc.c:1646  */
    break;

  case 12:
#line 61 "mini_l.y" /* yacc.c:1646  */
    {printf("i_loop -> identifier\n");}
#line 1452 "y.tab.c" /* yacc.c:1646  */
    break;

  case 13:
#line 63 "mini_l.y" /* yacc.c:1646  */
    {printf("statement -> var assign expression\n");}
#line 1458 "y.tab.c" /* yacc.c:1646  */
    break;

  case 14:
#line 64 "mini_l.y" /* yacc.c:1646  */
    {printf("statement -> if bool_expr then ss_loop endif\n");}
#line 1464 "y.tab.c" /* yacc.c:1646  */
    break;

  case 15:
#line 65 "mini_l.y" /* yacc.c:1646  */
    {printf("statement -> if bool_expr then ss_loop else endif\n");}
#line 1470 "y.tab.c" /* yacc.c:1646  */
    break;

  case 16:
#line 66 "mini_l.y" /* yacc.c:1646  */
    {printf("statement -> while bool_expr beginloop ss_loop endloop\n");}
#line 1476 "y.tab.c" /* yacc.c:1646  */
    break;

  case 17:
#line 67 "mini_l.y" /* yacc.c:1646  */
    {printf("statement -> do beginloop ss_loop endloop while bool_expr\n");}
#line 1482 "y.tab.c" /* yacc.c:1646  */
    break;

  case 18:
#line 68 "mini_l.y" /* yacc.c:1646  */
    {printf("statement -> read var_loop\n");}
#line 1488 "y.tab.c" /* yacc.c:1646  */
    break;

  case 19:
#line 69 "mini_l.y" /* yacc.c:1646  */
    {printf("statement -> write var_loop\n");}
#line 1494 "y.tab.c" /* yacc.c:1646  */
    break;

  case 20:
#line 70 "mini_l.y" /* yacc.c:1646  */
    {printf("statement -> continue\n");}
#line 1500 "y.tab.c" /* yacc.c:1646  */
    break;

  case 21:
#line 71 "mini_l.y" /* yacc.c:1646  */
    {printf("statement -> return experssion\n");}
#line 1506 "y.tab.c" /* yacc.c:1646  */
    break;

  case 22:
#line 73 "mini_l.y" /* yacc.c:1646  */
    {printf("var_loop -> var comma var_loop\n");}
#line 1512 "y.tab.c" /* yacc.c:1646  */
    break;

  case 23:
#line 74 "mini_l.y" /* yacc.c:1646  */
    {printf("var_loop -> var\n");}
#line 1518 "y.tab.c" /* yacc.c:1646  */
    break;

  case 24:
#line 76 "mini_l.y" /* yacc.c:1646  */
    {printf("bool_expr -> relation_and_expr ro_loop\n");}
#line 1524 "y.tab.c" /* yacc.c:1646  */
    break;

  case 26:
#line 78 "mini_l.y" /* yacc.c:1646  */
    {printf("ro_loop -> or relation_and_expr ro_loop\n");}
#line 1530 "y.tab.c" /* yacc.c:1646  */
    break;

  case 27:
#line 80 "mini_l.y" /* yacc.c:1646  */
    {printf("relation_and_expr -> relation_and_expr ra_loop\n");}
#line 1536 "y.tab.c" /* yacc.c:1646  */
    break;

  case 29:
#line 82 "mini_l.y" /* yacc.c:1646  */
    {printf("ra_loop -> or relation_expr ra_loop\n");}
#line 1542 "y.tab.c" /* yacc.c:1646  */
    break;

  case 30:
#line 84 "mini_l.y" /* yacc.c:1646  */
    {printf("relation_expr -> not n_loop\n");}
#line 1548 "y.tab.c" /* yacc.c:1646  */
    break;

  case 31:
#line 85 "mini_l.y" /* yacc.c:1646  */
    {printf("relation_expr -> n_loop\n");}
#line 1554 "y.tab.c" /* yacc.c:1646  */
    break;

  case 32:
#line 87 "mini_l.y" /* yacc.c:1646  */
    {printf("n_loop -> expression comp expression\n");}
#line 1560 "y.tab.c" /* yacc.c:1646  */
    break;

  case 33:
#line 88 "mini_l.y" /* yacc.c:1646  */
    {printf("n_loop -> true\n");}
#line 1566 "y.tab.c" /* yacc.c:1646  */
    break;

  case 34:
#line 89 "mini_l.y" /* yacc.c:1646  */
    {printf("n_loop -> false\n");}
#line 1572 "y.tab.c" /* yacc.c:1646  */
    break;

  case 35:
#line 90 "mini_l.y" /* yacc.c:1646  */
    {printf("n_loop -> l_paren bool_expr r_paren\n");}
#line 1578 "y.tab.c" /* yacc.c:1646  */
    break;

  case 36:
#line 92 "mini_l.y" /* yacc.c:1646  */
    {printf("comp -> eq\n");}
#line 1584 "y.tab.c" /* yacc.c:1646  */
    break;

  case 37:
#line 93 "mini_l.y" /* yacc.c:1646  */
    {printf("comp -> neq\n");}
#line 1590 "y.tab.c" /* yacc.c:1646  */
    break;

  case 38:
#line 94 "mini_l.y" /* yacc.c:1646  */
    {printf("comp -> lt\n");}
#line 1596 "y.tab.c" /* yacc.c:1646  */
    break;

  case 39:
#line 95 "mini_l.y" /* yacc.c:1646  */
    {printf("comp -> gt\n");}
#line 1602 "y.tab.c" /* yacc.c:1646  */
    break;

  case 40:
#line 96 "mini_l.y" /* yacc.c:1646  */
    {printf("comp -> lte\n");}
#line 1608 "y.tab.c" /* yacc.c:1646  */
    break;

  case 41:
#line 97 "mini_l.y" /* yacc.c:1646  */
    {printf("comp -> gte\n");}
#line 1614 "y.tab.c" /* yacc.c:1646  */
    break;

  case 42:
#line 99 "mini_l.y" /* yacc.c:1646  */
    {printf("expression -> multiplicative_expr as_loop\n");}
#line 1620 "y.tab.c" /* yacc.c:1646  */
    break;

  case 44:
#line 101 "mini_l.y" /* yacc.c:1646  */
    {printf("as_loop -> add multiplicative_expr as_loop\n");}
#line 1626 "y.tab.c" /* yacc.c:1646  */
    break;

  case 45:
#line 102 "mini_l.y" /* yacc.c:1646  */
    {printf("as_loop -> sub multiplicative_expr as_loop\n");}
#line 1632 "y.tab.c" /* yacc.c:1646  */
    break;

  case 46:
#line 104 "mini_l.y" /* yacc.c:1646  */
    {printf("multiplicative_expr -> term mdm_loop\n");}
#line 1638 "y.tab.c" /* yacc.c:1646  */
    break;

  case 48:
#line 106 "mini_l.y" /* yacc.c:1646  */
    {printf("mdm_loop -> mult term mdm_loop\n");}
#line 1644 "y.tab.c" /* yacc.c:1646  */
    break;

  case 49:
#line 107 "mini_l.y" /* yacc.c:1646  */
    {printf("mdm_loop -> div term mdm_loop\n");}
#line 1650 "y.tab.c" /* yacc.c:1646  */
    break;

  case 50:
#line 108 "mini_l.y" /* yacc.c:1646  */
    {printf("mdm_loop -> mod term mdm_loop\n");}
#line 1656 "y.tab.c" /* yacc.c:1646  */
    break;

  case 51:
#line 110 "mini_l.y" /* yacc.c:1646  */
    {printf("term -> identifier l_paren ec_loop r_paren\n");}
#line 1662 "y.tab.c" /* yacc.c:1646  */
    break;

  case 52:
#line 111 "mini_l.y" /* yacc.c:1646  */
    {printf("term -> sub s_loop\n");}
#line 1668 "y.tab.c" /* yacc.c:1646  */
    break;

  case 53:
#line 112 "mini_l.y" /* yacc.c:1646  */
    {printf("term -> s_loop\n");}
#line 1674 "y.tab.c" /* yacc.c:1646  */
    break;

  case 55:
#line 114 "mini_l.y" /* yacc.c:1646  */
    {printf("ec_loop -> expression comma ec_loop\n");}
#line 1680 "y.tab.c" /* yacc.c:1646  */
    break;

  case 56:
#line 115 "mini_l.y" /* yacc.c:1646  */
    {printf("ec_loop -> expression\n");}
#line 1686 "y.tab.c" /* yacc.c:1646  */
    break;

  case 57:
#line 117 "mini_l.y" /* yacc.c:1646  */
    {printf("s_loop -> l_paren expression r_paren\n");}
#line 1692 "y.tab.c" /* yacc.c:1646  */
    break;

  case 58:
#line 118 "mini_l.y" /* yacc.c:1646  */
    {printf("s_loop -> number\n");}
#line 1698 "y.tab.c" /* yacc.c:1646  */
    break;

  case 59:
#line 119 "mini_l.y" /* yacc.c:1646  */
    {printf("s_loop -> var\n");}
#line 1704 "y.tab.c" /* yacc.c:1646  */
    break;

  case 60:
#line 121 "mini_l.y" /* yacc.c:1646  */
    {printf("var -> identifier l_bracket expression r_bracket\n");}
#line 1710 "y.tab.c" /* yacc.c:1646  */
    break;

  case 61:
#line 122 "mini_l.y" /* yacc.c:1646  */
    {printf("var -> identifier\n");}
#line 1716 "y.tab.c" /* yacc.c:1646  */
    break;

  case 62:
#line 125 "mini_l.y" /* yacc.c:1646  */
    {printf("function -> FUNCTION\n");}
#line 1722 "y.tab.c" /* yacc.c:1646  */
    break;

  case 63:
#line 127 "mini_l.y" /* yacc.c:1646  */
    {printf("identifier -> IDENT %s\n", (yyvsp[0].idval));}
#line 1728 "y.tab.c" /* yacc.c:1646  */
    break;

  case 64:
#line 129 "mini_l.y" /* yacc.c:1646  */
    {printf("semicolon -> SEMICOLON\n");}
#line 1734 "y.tab.c" /* yacc.c:1646  */
    break;

  case 65:
#line 131 "mini_l.y" /* yacc.c:1646  */
    {printf("beginparams -> BEGIN_PARAMS\n");}
#line 1740 "y.tab.c" /* yacc.c:1646  */
    break;

  case 66:
#line 133 "mini_l.y" /* yacc.c:1646  */
    {printf("endparams -> END_PARAMS\n");}
#line 1746 "y.tab.c" /* yacc.c:1646  */
    break;

  case 67:
#line 135 "mini_l.y" /* yacc.c:1646  */
    {printf("beginlocals -> BEGIN_LOCALS\n");}
#line 1752 "y.tab.c" /* yacc.c:1646  */
    break;

  case 68:
#line 137 "mini_l.y" /* yacc.c:1646  */
    {printf("endlocals -> END_LOCALS\n");}
#line 1758 "y.tab.c" /* yacc.c:1646  */
    break;

  case 69:
#line 139 "mini_l.y" /* yacc.c:1646  */
    {printf("beginbody -> BEGIN_BODY\n");}
#line 1764 "y.tab.c" /* yacc.c:1646  */
    break;

  case 70:
#line 141 "mini_l.y" /* yacc.c:1646  */
    {printf("endbody -> END_BODY\n");}
#line 1770 "y.tab.c" /* yacc.c:1646  */
    break;

  case 71:
#line 143 "mini_l.y" /* yacc.c:1646  */
    {printf("comma -> COMMA\n");}
#line 1776 "y.tab.c" /* yacc.c:1646  */
    break;

  case 72:
#line 145 "mini_l.y" /* yacc.c:1646  */
    {printf("colon -> COLON\n");}
#line 1782 "y.tab.c" /* yacc.c:1646  */
    break;

  case 73:
#line 147 "mini_l.y" /* yacc.c:1646  */
    {printf("integer -> INTEGER\n");}
#line 1788 "y.tab.c" /* yacc.c:1646  */
    break;

  case 74:
#line 149 "mini_l.y" /* yacc.c:1646  */
    {printf("array -> ARRAY\n");}
#line 1794 "y.tab.c" /* yacc.c:1646  */
    break;

  case 75:
#line 151 "mini_l.y" /* yacc.c:1646  */
    {printf("l_bracket -> L_BRACKET\n");}
#line 1800 "y.tab.c" /* yacc.c:1646  */
    break;

  case 76:
#line 153 "mini_l.y" /* yacc.c:1646  */
    {printf("number -> NUMBER %f\n", (yyvsp[0].nval));}
#line 1806 "y.tab.c" /* yacc.c:1646  */
    break;

  case 77:
#line 155 "mini_l.y" /* yacc.c:1646  */
    {printf("r_bracket -> R_BRACKET\n");}
#line 1812 "y.tab.c" /* yacc.c:1646  */
    break;

  case 78:
#line 157 "mini_l.y" /* yacc.c:1646  */
    {printf("of -> OF\n");}
#line 1818 "y.tab.c" /* yacc.c:1646  */
    break;

  case 79:
#line 159 "mini_l.y" /* yacc.c:1646  */
    {printf("assign -> ASSIGN\n");}
#line 1824 "y.tab.c" /* yacc.c:1646  */
    break;

  case 80:
#line 161 "mini_l.y" /* yacc.c:1646  */
    {printf("if -> IF\n");}
#line 1830 "y.tab.c" /* yacc.c:1646  */
    break;

  case 81:
#line 163 "mini_l.y" /* yacc.c:1646  */
    {printf("then -> THEN\n");}
#line 1836 "y.tab.c" /* yacc.c:1646  */
    break;

  case 82:
#line 165 "mini_l.y" /* yacc.c:1646  */
    {printf("else -> ELSE\n");}
#line 1842 "y.tab.c" /* yacc.c:1646  */
    break;

  case 83:
#line 167 "mini_l.y" /* yacc.c:1646  */
    {printf("endif -> ENDIF\n");}
#line 1848 "y.tab.c" /* yacc.c:1646  */
    break;

  case 84:
#line 169 "mini_l.y" /* yacc.c:1646  */
    {printf("while -> WHILE\n");}
#line 1854 "y.tab.c" /* yacc.c:1646  */
    break;

  case 85:
#line 171 "mini_l.y" /* yacc.c:1646  */
    {printf("beginloop -> BEGINLOOP\n");}
#line 1860 "y.tab.c" /* yacc.c:1646  */
    break;

  case 86:
#line 173 "mini_l.y" /* yacc.c:1646  */
    {printf("endloop -> ENDLOOP\n");}
#line 1866 "y.tab.c" /* yacc.c:1646  */
    break;

  case 87:
#line 175 "mini_l.y" /* yacc.c:1646  */
    {printf("do -> DO\n");}
#line 1872 "y.tab.c" /* yacc.c:1646  */
    break;

  case 88:
#line 177 "mini_l.y" /* yacc.c:1646  */
    {printf("read -> READ\n");}
#line 1878 "y.tab.c" /* yacc.c:1646  */
    break;

  case 89:
#line 179 "mini_l.y" /* yacc.c:1646  */
    {printf("write -> WRITE\n");}
#line 1884 "y.tab.c" /* yacc.c:1646  */
    break;

  case 90:
#line 181 "mini_l.y" /* yacc.c:1646  */
    {printf("continue -> CONTINUE\n");}
#line 1890 "y.tab.c" /* yacc.c:1646  */
    break;

  case 91:
#line 183 "mini_l.y" /* yacc.c:1646  */
    {printf("return -> RETURN\n");}
#line 1896 "y.tab.c" /* yacc.c:1646  */
    break;

  case 92:
#line 185 "mini_l.y" /* yacc.c:1646  */
    {printf("or -> OR\n");}
#line 1902 "y.tab.c" /* yacc.c:1646  */
    break;

  case 93:
#line 187 "mini_l.y" /* yacc.c:1646  */
    {printf("and -> AND\n");}
#line 1908 "y.tab.c" /* yacc.c:1646  */
    break;

  case 94:
#line 189 "mini_l.y" /* yacc.c:1646  */
    {printf("not -> NOT\n");}
#line 1914 "y.tab.c" /* yacc.c:1646  */
    break;

  case 95:
#line 191 "mini_l.y" /* yacc.c:1646  */
    {printf("true -> TRUE\n");}
#line 1920 "y.tab.c" /* yacc.c:1646  */
    break;

  case 96:
#line 193 "mini_l.y" /* yacc.c:1646  */
    {printf("false -> FALSE\n");}
#line 1926 "y.tab.c" /* yacc.c:1646  */
    break;

  case 97:
#line 195 "mini_l.y" /* yacc.c:1646  */
    {printf("l_paren -> L_PAREN\n");}
#line 1932 "y.tab.c" /* yacc.c:1646  */
    break;

  case 98:
#line 197 "mini_l.y" /* yacc.c:1646  */
    {printf("r_paren -> R_PAREN\n");}
#line 1938 "y.tab.c" /* yacc.c:1646  */
    break;

  case 99:
#line 199 "mini_l.y" /* yacc.c:1646  */
    {printf("eq -> EQ\n");}
#line 1944 "y.tab.c" /* yacc.c:1646  */
    break;

  case 100:
#line 201 "mini_l.y" /* yacc.c:1646  */
    {printf("neq -> NEQ\n");}
#line 1950 "y.tab.c" /* yacc.c:1646  */
    break;

  case 101:
#line 203 "mini_l.y" /* yacc.c:1646  */
    {printf("lt -> LT\n");}
#line 1956 "y.tab.c" /* yacc.c:1646  */
    break;

  case 102:
#line 205 "mini_l.y" /* yacc.c:1646  */
    {printf("gt -> GT\n");}
#line 1962 "y.tab.c" /* yacc.c:1646  */
    break;

  case 103:
#line 207 "mini_l.y" /* yacc.c:1646  */
    {printf("lte -> LTE\n");}
#line 1968 "y.tab.c" /* yacc.c:1646  */
    break;

  case 104:
#line 209 "mini_l.y" /* yacc.c:1646  */
    {printf("gte -> GTE\n");}
#line 1974 "y.tab.c" /* yacc.c:1646  */
    break;

  case 105:
#line 211 "mini_l.y" /* yacc.c:1646  */
    {printf("add -> ADD\n");}
#line 1980 "y.tab.c" /* yacc.c:1646  */
    break;

  case 106:
#line 213 "mini_l.y" /* yacc.c:1646  */
    {printf("sub -> SUB\n");}
#line 1986 "y.tab.c" /* yacc.c:1646  */
    break;

  case 107:
#line 215 "mini_l.y" /* yacc.c:1646  */
    {printf("mult -> MULT\n");}
#line 1992 "y.tab.c" /* yacc.c:1646  */
    break;

  case 108:
#line 217 "mini_l.y" /* yacc.c:1646  */
    {printf("div -> DIV\n");}
#line 1998 "y.tab.c" /* yacc.c:1646  */
    break;

  case 109:
#line 219 "mini_l.y" /* yacc.c:1646  */
    {printf("mod -> MOD\n");}
#line 2004 "y.tab.c" /* yacc.c:1646  */
    break;


#line 2008 "y.tab.c" /* yacc.c:1646  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 221 "mini_l.y" /* yacc.c:1906  */

int main(int argc, char ** argv)
{
   if(argc >= 2)
   {
      yyin = fopen(argv[1], "r");
      if(yyin == NULL)
      {
         yyin = stdin;
      }
   }
   else
   {
      yyin = stdin;
   }
   yyparse();
   return 0;
}
void yyerror(const char *msg) {
   printf("** Line %d, position %d: %s\n", currentLine, currentPosition, msg);
}
