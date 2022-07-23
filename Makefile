build: clean coletor java

coletor:
	erlc -o Colector/ Colector/tcpHandler.erl Colector/loginManager.erl

java:
	javac -cp .:dependencies/jar/jeromq-0.5.2.jar Aggregator/*.java
	javac -cp .:dependencies/jar/jeromq-0.5.2.jar Client/*.java
	javac Device/*.java
	
clean:
	-@rm Aggregator/*.class
	-@rm Colector/*.beam
	-@rm Client/*.class
	-@rm Device/*.class
