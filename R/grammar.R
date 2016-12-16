lyst <- function(...) {
  args <- list(...)
  if (length(args) == 0) return(list())
  names(args) <- vapply(names(args),
                        function(nm) {
                          if (is.null(.type[[nm]])) {
                            stop(noquote(nm), ' not in .type', call. = FALSE)
                          } else {
                            .type[[nm]]
                          }
                        }, numeric(1))
  args
}

.actions <- with(.type, lyst(
  STATEMENT_LIST = lyst(
    REXPRESSION = lyst(
      SHIFT = TRUE,
      REXPRESSION = lyst(
        REDUCE = list(from = c(STATEMENT_LIST, REXPRESSION), to = STATEMENT_LIST)
      )
    ),
    TINSEL_COMMENT = lyst(
      SHIFT = TRUE,
      IDENTIFIER = lyst(
        REDUCE = list(from = TINSEL_COMMENT, to = DECORATORS)
      )
    ),
    TINSEL_STATEMENT = lyst(
      REXPRESSION = lyst(
        REDUCE = list(from = c(STATEMENT_LIST, TINSEL_STATEMENT),
                      to = STATEMENT_LIST)
      )
    ),
    DECORATORS = lyst(
      DECORATEE = lyst(
        REXPRESSION = lyst(
          REDUCE = list(from = c(DECORATORS, DECORATEE),
                        to = TINSEL_STATEMENT)
        ),
        TINSEL_COMMENT = lyst(
          REDUCE = list(from = c(DECORATORS, DECORATEE),
                        to = TINSEL_STATEMENT)
        )
      ),
      IDENTIFIER = lyst(
        SHIFT = TRUE,
        ASSIGN_OPERATOR = lyst(
          REDUCE = list(from = IDENTIFIER, to = FUNCTION_NAME)
        ),
        EXTRACT_OPERATOR = lyst(
          SHIFT = TRUE,
          IDENTIFIER = lyst(
            REDUCE = list(from = c(IDENTIFIER, EXTRACT_OPERATOR),
                          to = FILE_REFERENCE)
          )
        ),
        IDENTIFIER = lyst(
          REDUCE = list(from = IDENTIFIER, to = FUNCTION_REFERENCE)
        )
      ),
      FILE_REFERENCE = lyst(
        IDENTIFIER = lyst(
          SHIFT = TRUE,
          IDENTIFIER = lyst(
            REDUCE = list(from = IDENTIFIER, to = FUNCTION_NAME),
            LPAREN = lyst(
              REDUCE = list(from = IDENTIFIER, to = FUNCTION_NAME)
            )
          )
        ),
        FUNCTION_NAME = lyst(
          IDENTIFIER = lyst(
            REDUCE = list(from = c(FILE_REFERENCE, to = FUNCTION_NAME))
          ),
          LPAREN = lyst(
            SHIFT = TRUE,
            RPAREN = lyst(
              REDUCE = list(from = LPAREN, to = ARGUMENT_LIST)
            ),
            REXPRESSION = lyst(
              REDUCE = list(from = LPAREN, to = ARGUMENT_LIST)
            ),
            IDENTIFIER = lyst(
              REDUCE = list(from = LPAREN, to = ARGUMENT_LIST)
            )
          ),
          ARGUMENT_LIST = lyst(
            RPAREN = lyst(
              SHIFT = TRUE,
              IDENTIFIER = lyst(
                REDUCE = list(from = c(FILE_REFERENCE, FUNCTION_NAME, ARGUMENT_LIST, RPAREN),
                              to = FUNCTION_REFERENCE)
              )
            ),
            IDENTIFIER = lyst(
              SHIFT = TRUE,
              ASSIGN_OPERATOR = lyst(
                REDUCE = list(from = IDENTIFIER, to = PARAMETER_NAME)
              ),
              IDENTIFIER = lyst(
                REDUCE = list(from = IDENTIFIER, to = REXPRESSION)
              ),
              RPAREN = lyst(
                REDUCE = list(from = IDENTIFIER, to = REXPRESSION)
              )
            ),
            REXPRESSION = lyst(
              SHIFT = TRUE,
              IDENTIFIER = lyst(
                REDUCE = list(from = REXPRESSION, to = ARGUMENT)
              ),
              REXPRESSION = lyst(
                REDUCE = list(from = REXPRESSION, to = ARGUMENT)
              ),
              RPAREN = lyst(
                REDUCE = list(from = c(ARGUMENT_LIST, ARGUMENT),
                              to = ARGUMENT_LIST)
              )
            ),
            PARAMETER_NAME = lyst(
              ASSIGN_OPERATOR = lyst(
                SHIFT = TRUE,
                REXPRESSION = lyst(
                  SHIFT = TRUE,
                  IDENTIFIER = lyst(
                    REDUCE = list(from = c(PARAMETER_NAME, ASSIGN_OPERATOR, REXPRESSION),
                                  to = ARGUMENT)
                  ),
                  REXPRESSION = lyst(
                    REDUCE = list(from = c(PARAMETER_NAME, ASSIGN_OPERATOR, REXPRESSION),
                                  to = ARGUMENT)
                  )
                )
              )
            ),
            ARGUMENT = lyst(
              IDENTIFIER = lyst(
                REDUCE = list(from = c(ARGUMENT_LIST, ARGUMENT),
                              to = ARGUMENT_LIST)
              ),
              REXPRESSION = lyst(
                REDUCE = list(from = c(ARGUMENT_LIST, ARGUMENT),
                              to = ARGUMENT_LIST)
              ),
              RPAREN = lyst(
                REDUCE = list(from = c(ARGUMENT_LIST, ARGUMENT),
                              to = ARGUMENT_LIST)
              )
            )
          )
        )
      ),
      FUNCTION_REFERENCE = lyst(
        IDENTIFIER = lyst(
          REDUCE = list(from = FUNCTION_REFERENCE, to = DECORATOR)
        )
      ),
      FUNCTION_NAME = lyst(
        ASSIGN_OPERATOR = lyst(
          SHIFT = TRUE,
          IDENTIFIER = lyst(
            SHIFT = TRUE,
            REXPRESSION = lyst(
              REDUCE = list(from = IDENTIFIER, to = FUNCTION_REFERENCE)
            ),
            FUNCTION_REFERENCE = lyst(
              REXPRESSION = lyst(
                REDUCE = list(from = c(FUNCTION_NAME, ASSIGN_OPERATOR, FUNCTION_REFERENCE),
                              to = DECORATEE)
              ),
              TINSEL_COMMENT = lyst(
                REDUCE = list(from = c(FUNCTION_NAME, ASSIGN_OPERATOR, FUNCTION_REFERENCE),
                              to = DECORATEE)
              )
            )
          ),
          FUNCTION_OPERATOR = lyst(
            SHIFT = TRUE,
            LPAREN = lyst(
              SHIFT = TRUE,
              RPAREN = lyst(
                REDUCE = list(from = LPAREN, to = PARAMETER_LIST)
              ),
              IDENTIFIER = lyst(
                REDUCE = list(form = LPAREN, to = PARAMETER_LIST)
              )
            ),
            PARAMETER_LIST = lyst(
              PARAMETER = lyst(
                RPAREN = lyst(
                  REDUCE = list(from = c(PARAMETER_LIST, PARAMETER),
                                to = PARAMETER_LIST)
                )
              ),
              IDENTIFIER = lyst(
                SHIFT = TRUE,
                ASSIGN_OPERATOR = lyst(
                  REDUCE = list(from = IDENTIFIER, to = PARAMETER_NAME)
                )
              ),
              PARAMETER_NAME = lyst(
                ASSIGN_OPERATOR = lyst(
                  SHIFT = TRUE,
                  REXPRESSION = lyst(
                    SHIFT = TRUE,
                    RPAREN = lyst(
                      REDUCE = list(from = c(PARAMETER_NAME, ASSIGN_OPERATOR, REXPRESSION),
                                    to = PARAMETER)
                    )
                  )
                )
              ),
              RPAREN = lyst(
                SHIFT = TRUE,
                REXPRESSION = lyst(
                  REDUCE = list(from = c(PARAMETER_LIST, RPAREN),
                                to = PARAMETER_LIST)
                )
              ),
              REXPRESSION = lyst(
                SHIFT = TRUE,
                REXPRESSION = lyst(
                  REDUCE = list(from = c(FUNCTION_OPERATOR, PARAMETER_LIST, REXPRESSION),
                                to = FUNCTION_DEFINITION)
                )
              )
            )
          ),
          FUNCTION_DEFINITION = lyst(
            REXPRESSION = lyst(
              REDUCE = list(from = c(FUNCTION_NAME, ASSIGN_OPERATOR, FUNCTION_DEFINITION),
                            to = DECORATEE)
            )
          )
        ),
        IDENTIFIER = lyst(
          SHIFT = TRUE
        )
      ),
      DECORATOR = lyst(
        IDENTIFIER = lyst(
          REDUCE = list(from = c(DECORATORS, DECORATOR), to = DECORATORS)
        )
      )
    )
  )
))

# Program => StatementList
# StatementList => StatementList Statement | Statement
# Statement => TinselStatement | RExpression
# TinselStatement => Decorators Decoratee
# Decorators => Decorators Decorator | Decorator
# Decorator => #. FileReference FunctionReference
# FileReference => FileName $ | <empty>
# FunctionReference => Identifier ( ArgumentList ) | Identifier
# Identifier => SyntacticName | NonsyntacticName
# ArgumentList => ArgumentList, Argument | Argument, ArgumentList | Argument
# Argument => ParameterName = ArgumentValue | ArgumentValue
# ArgumentValue => RExpression | <empty>
# Decoratee => Identifier AssignOp FunctionCall
# FunctionCall => FunctionDefinition | FunctionReference
# FunctionDefinition => FunctionOp ( Parameters ) RExpression
# Parameters => ParameterList | <empty>
# ParameterList => ParameterList, Parameter | Parameter
# Parameter => ParameterName = ParameterDefault | ParameterName
# ParameterName => Identifier
# ParameterDefault => RExpression
# FunctionOp => "function"
# AssignOp => "=" | "<-"


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
