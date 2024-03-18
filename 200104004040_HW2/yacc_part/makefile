FLEX = flex

YACC = yacc

COMPILER = gcc


GCC_FLAG = -ll -w


YACC_HELPER = yacc.h


LEXER = gpp_lexer

INTERPRETER = gpp_interpreter


OUTPUT = gpp_interpreter


INPUTFILE = input.g++

flex: ${LEXER}.l ${INTERPRETER}.y ${YACC_HELPER}
	${YACC} -d -o ${INTERPRETER}.c ${INTERPRETER}.y
	${FLEX} -o ${LEXER}.c $(LEXER).l 
	${COMPILER} ${LEXER}.c ${INTERPRETER}.c  -o ${OUTPUT} ${GCC_FLAG}
	./${OUTPUT}

input:
	./${OUTPUT} ${INPUTFILE}


