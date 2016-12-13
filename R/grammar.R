lyst <- function(...) {
  args <- list(...)
  if (length(args) == 0) return(list())
  evlenv <- list2env(.type, parent = emptyenv())
  evlenv$lookahead <- 'lookahead'
  evald <- vapply(names(args), get, numeric(1), envir = evlenv, inherits = FALSE)
  names(args) <- evald
  args
}

.reduce <- lyst(
  TINSEL_COMMENT = lyst(
    IDENTIFIER = list(reduce = .type$TINSEL_COMMENT, to = .type$DECORATORS)
  ),
  FILE_NAME = lyst(),
  DECORATORS = lyst(
    IDENTIFIER = lyst(
      EXTRACTOP = list(reduce = .type$IDENTIFIER, to = .type$FILE_NAME),
      ASSIGNOP = list(reduce = .type$DECORATEE, to = .type$IDENTIFIER)
    ),
    FILE_NAME = lyst(
      EXTRACTOP = lyst(
        IDENTIFIER = list(reduce = c(.type$FILE_NAME, .type$EXTRACTOP), to = .type$FILE_REFERENCE)
      )
    ),
    FILE_REFERENCE = lyst(
      IDENTIFIER = lyst(
        IDENTIFIER = list(reduce = .type$IDENTIFIER, to = .type$FUNCTION_NAME)
      ),
      FUNCTION_NAME = lyst(
        IDENTIFIER = list(reduce = c(.type$FILE_REFERENCE, .type$FUNCTION_NAME), to = .type$FUNCTION_REFERENCE)
      )
    ),
    FUNCTION_REFERENCE = lyst(
      LPAREN = lyst(
        IDENTIFIER = list(reduce = .type$LPAREN, to = .type$ARGUMENTS)
      ),
      ARGUMENTS = lyst(
        IDENTIFIER = lyst(
          ASSIGNOP = lyst(
            EXPRESSION = lyst(
              IDENTIFIER = list(reduce = c(.type$IDENTIFIER, .type$ASSIGNOP, .type$EXPRESSION), to = .type$ASSIGNMENT)
            )
          )
        ),
        ASSIGNMENT = lyst(
          IDENTIFIER = list(reduce = c(.type$ARGUMENTS, .type$ASSIGNMENT), to = .type$ARGUMENTS)
        ),
        EXPRESSION = lyst(
          IDENTIFIER = list(reduce = c(.type$ARGUMENTS, .type$EXPRESSION), to = .type$ARGUMENTS)
        ),
        RPAREN = lyst(
          TINSEL_COMMENT = list(reduce = c(.type$ARGUMENTS, .type$RPAREN), to = .type$ARGUMENTS)
        ),
        TINSEL_COMMENT = list(reduce = c(.type$FUNCTION_REFERENCE, .type$ARGUMENTS), to = .type$DECORATOR)
      )
    ),
    DECORATOR = lyst(
      TINSEL_COMMENT = list(reduce = c(.type$DECORATORS, .type$DECORATED), to = .type$DECORATORS),
      IDENTIFIER = list(reduce = c(.type$DECORATORS, .type$DECORATOR), to = .type$DECORATORS)
    )
  )
)

.shift <- lyst(
  LOOKAHEAD = .type$TINSEL_COMMENT,
  DECORATORS = lyst(
    LOOKAHEAD = .type$IDENTIFIER,
    FILE_NAME = lyst(
      LOOKAHEAD = .type$FILE_ACCESSOR
    ),
    FILE_REFERENCE = lyst(
      LOOKAHEAD = .type$IDENTIFIER
    ),
    FUNCTION_REFERENCE = lyst(
      LOOKAHEAD = .type$LPAREN,
      ARGUMENTS = lyst(
        LOOKAHEAD = c(.type$IDENTIFIER, .type$EXPRESSION, .type$RPAREN),
        IDENTIFIER = lyst(
          LOOKAHEAD = .type$EQUALS,
          EQUALS = lyst(
            LOOKAHEAD = .type$EXPRESSION
          )
        )
      )
    ),
    DECORATOR = lyst(
      DECORATEE = lyst(
        LOOKAHEAD = .type$ASSIGNOP,
        ASSIGNOP = lyst(
          LOOKAHEAD = .type$FUNCTION_STMT
        )
      )
    )
  )
)

parse_tbl <- function() {
  tbl <- as.data.frame(rbind(
    list(~ list(), .type$TINSEL_COMMENT, ~ NULL),
    list(~ .type$TINSEL_COMMENT, .type$IDENTIFIER, .type$DECORATOR ~ .type$TINSEL_COMMENT),
    list(~ .type$DECORATORS, .type$IDENTIFIER, ~ NULL),
    list(~ .type$DECORATORS + .type$IDENTIFIER, .type$FILE_ACCESSOR, .type$FILE_NAME ~ .type$IDENTIFIER),
    list(~ .type$DECORATORS + .type$FILE_NAME, .type$FILE_ACCESSOR, ~ NULL),
    list(~ .type$DECORATORS + .type$FILE_NAME + .type$FILE_ACCESSOR, .type$IDENTIFIER, .type$FILE_REFERENCE ~ .type$FILE_NAME + .type$FILE_ACCESSOR),
    list(~ .type$DECORATORS + .type$FILE_REFERENCE, .type$IDENTIFIER, ~ NULL),
    list(~ .type$DECORATORS + .type$FILE_REFERENCE + .type$IDENTIFIER, .type$IDENTIFIER, .type$FUNCTION_NAME ~ .type$IDENTIFIER),
    list(~ .type$DECORATORS + .type$FILE_REFERENCE + .type$FUNCTION_NAME, .type$IDENTIFIER, .type$FUNCTION_REFERENCE ~ .type$FILE_REFERENCE + .type$FUNCTION_NAME),
    list(~ .type$DECORATORS + .type$FUNCTION_REFERENCE, .type$LPAREN, ~ NULL),
    list(~ .type$DECORATORS + .type$FUNCTION_REFERENCE + .type$LPAREN, .type$IDENTIFIER, .type$ARGUMENTS ~ .type$LPAREN),
    list(~ .type$DECORATORS + .type$FUNCTION_REFERENCE + .type$ARGUMENTS, .type$IDENTIFIER, ~ NULL),
    list(~ .type$DECORATORS + .type$FUNCTION_REFERENCE + .type$ARGUMENTS + .type$IDENTIFIER, .type$EQUALS, ~ NULL),
    list(~ .type$DECORATORS + .type$FUNCTION_REFERENCE + .type$ARGUMENTS + .type$IDENTIFIER + .type$EQUALS, .type$EXPRESSION, ~ NULL),
    list(
      ~ .type$DECORATORS + .type$FUNCTION_REFERENCE + .type$ARGUMENTS + .type$IDENTIFIER + .type$EQUALS + .type$EXPRESSION,
      .type$IDENTIFIER,
      .type$ASSIGNMENT ~ .type$IDENTIFIER + .type$EQUALS + .type$EXPRESSION
    ),
    list(~ .type$DECORATORS + .type$FUNCTION_REFERENCE + .type$ARGUMENTS + .type$ASSIGNMENT, .type$IDENTIFIER, .type$ARGUMENTS ~ .type$ARGUMENTS + .type$ASSIGNMENT),
    list(~ .type$DECORATORS + .type$FUNCTION_REFERENCE + .type$ARGUMENTS, .type$EXPRESSION, ~ NULL),
    list(~ .type$DECORATORS + .type$FUNCTION_REFERENCE + .type$ARGUMENTS + .type$EXPRESSION, .type$IDENTIFIER, .type$ARGUMENTS ~ .type$ARGUMENTS + .type$EXPRESSION),
    list(~ .type$DECORATORS + .type$FUNCTION_REFERENCE + .type$ARGUMENTS, .type$RPAREN, ~ NULL),
    list(~ .type$DECORATORS + .type$FUNCTION_REFERENCE + .type$ARGUMENTS + .type$RPAREN, .type$TINSEL_COMMENT, .type$ARGUMENTS ~ .type$ARGUMENTS + .type$RPAREN),
    list(~ .type$DECORATORS + .type$FUNCTION_REFERENCE + .type$ARGUMENTS, .type$TINSEL_COMMENT, .type$DECORATOR ~ .type$FUNCTION_REFERENCE + .type$ARGUMENTS),
    list(~ .type$DECORATORS + .type$DECORATOR, .type$TINSEL_COMMENT, .type$DECORATORS ~ .type$DECORATORS + .type$DECORATOR),
    list(~ .type$DECORATORS + .type$DECORATOR, .type$IDENTIFIER, .type$DECORATORS ~ .type$DECORATORS + .type$DECORATORS),
    list(~ .type$DECORATORS + .type$IDENTIFIER, .type$ASSIGNOP, .type$IDENTIFIER ~ .type$DECORATEE),
    list(~ .type$DECORATORS + .type$DECORATOR + .type$DECORATEE, .type$ASSIGNOP, ~ NULL),
    list(~ .type$DECORATORS + .type$DECORATOR + .type$DECORATEE + .type$ASSIGNOP, .type$FUNCTION_STMT, ~ NULL),
    list(~ .type$DECORATORS + .type$DECORATOR + .type$DECORATEE + .type$ASSIGNOP + .type$FUNCTION_STMT, .type$LPAREN, c(NULL, NULL))
  ))
  colnames(tbl) <- c('ParseStack', 'LookAhead', 'ParserAction')
  tbl
}

# start => S
#
# S => A | A S
#
# A => B | C
#
# B => D E | E
#
# C => `identifier`
#
# D => G | G G
#
# E => `R expression`
#
# G => H I J | H J |
#
# H => #.
#
# I => J | M
#
# J => N K L
#
# K => P Q R | M
#
# L => `$`
#
# M => `nil`
#
# N => O | M
#
# O => `file`
#
# P => `[`
#
# Q => `digit` | `digit`P
#
# R => `]`
#
# U => V | W
#
# V => `package`::`function` | `function`
#
# W => X Y Z | M
#
# X => `(`
#
# Y => AA | M
#
# Z => `)`
#
# AA => E | E BB
#
# BB => `,`



### Examples

# #. fileA$export1
# #. fileB$export1
# function

# #. fileA$export1
# #. fileB$export1
# #. fileA$export2
# function

# #. other1
# function

# #. FileA:export1
# function


