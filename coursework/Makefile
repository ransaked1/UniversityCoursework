
CC = javac

BUGSPATH = bugs/
BUILDINGPATH = building/
STUDENTSPATH = students/

BUGSSRC = bugs/*.java
BUILDINGSRC = building/*.java
STUDENTSRC = students/*.java

FLAGS = -d

RM = rm -rf

SRC = *.java

CLASS = *.class

all: $(NAME)
	@echo "MAKE ALL IS DONE"

$(NAME):
	@$(CC) $(FLAGS) $(BUILDINGPATH) $(BUGSSRC)
	@$(CC) $(FLAGS) $(STUDENTSPATH) $(BUGSSRC)
	@$(CC) $(BUGSSRC)
	@$(CC) $(FLAGS) $(STUDENTSPATH) $(BUILDINGSRC)
	@$(CC) $(BUILDINGSRC)
	@$(CC) $(STUDENTSRC)
	@$(CC) Main.java

clean:
	@$(RM) $(OBJDIR)
	@echo "MAKE CLEAN IS DONE"

fclean: clean
	@$(RM) $(NAME)
	@echo "MAKE FCLEAN IS DONE"

re: fclean all
	@echo "MAKE RE IS DONE"

.PHONY: clean fclean