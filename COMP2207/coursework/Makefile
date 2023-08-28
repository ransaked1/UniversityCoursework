#!/bin/bash

RM=rm -rf

.PHONY: controller dstore client clean fclean

controller:
	@$(RM) Protocol.class Logger.class CLogger.class Index.class Controller.class
	@javac Protocol.java Logger.java CLogger.java Index.java Controller.java
	@java Controller 12355 $(R) 5000 15000
	@echo "MAKE CONTROLLER IS DONE"

dstore:
	@$(RM) Protocol.class Logger.class DLogger.class Dstore.class
	@javac Protocol.java Logger.java DLogger.java Dstore.java
	@java Dstore $(P) 12355 5000 $(FOLDER)
	@echo "MAKE DSTORE IS DONE"

client:
	@$(RM) ClientMain.class
	@javac -cp client.jar ClientMain.java
	@java -cp client.jar:. ClientMain 12355 1000
	@echo "MAKE CLIENT IS DONE"

clean:
	@$(RM) *.class
	@echo "MAKE CLEAN IS DONE"

fclean: clean
	@$(RM) *.log
	@$(RM) downloads/*.txt
	@$(RM) dstore*
	@echo "MAKE FCLEAN IS DONE"