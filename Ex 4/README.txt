-- Steps required to run the virtual machine:

1. Use assembler.py to compile the test.asb assembly file to a file readable by the machine.
	$ python3 assembler.py test.asb test.b

2. Compile the ex4.cpp source code using g++ compiler, to create the virtual machine executable. 
	$ g++ ex4.cpp -o ex4

3. Run the ex4 executable with the previously created file (step 1) as argument.
	$ ./ex4 test.b

-- Usage example:

	$ ./ex4 test.b
	Hello world!
	*****************
	0.000000