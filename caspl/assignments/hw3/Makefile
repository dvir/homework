CC		:=	gcc -m32
CC_FLAGS	:=	-Wall -g
ASM		:=	nasm
ASM_FLAGS	:=	-f elf -g
LINK		:=	ld

SRC_DIR		:=	src
OBJ_DIR		:=	obj
LIST_DIR	:=	list
BIN_DIR		:=	bin

all: ass3 

ass3: $(OBJ_DIR)/ass3.o $(OBJ_DIR)/coroutines.o $(OBJ_DIR)/scheduler.o $(OBJ_DIR)/printer.o
	$(LINK) -melf_i386 -o $(BIN_DIR)/ass3 $(OBJ_DIR)/ass3.o $(OBJ_DIR)/coroutines.o $(OBJ_DIR)/scheduler.o $(OBJ_DIR)/printer.o
	cp $(BIN_DIR)/ass3 ./ass3

# .c/.s compile rules
$(OBJ_DIR)/%.o : $(SRC_DIR)/%.c
	$(CC) -c $(CC_FLAGS) $< -o $@

$(OBJ_DIR)/%.o : $(SRC_DIR)/%.s
	$(ASM) $(ASM_FLAGS) $< -o $@ -l $(subst .o,.lst,$(subst $(OBJ_DIR),$(LIST_DIR),$@))

clean:
	rm -f $(BIN_DIR)/*.bin $(OBJ_DIR)/*.o $(LIST_DIR)/*.lst $(BIN_DIR)/ass3
	rm ./ass3
