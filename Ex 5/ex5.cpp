#include <iostream>
#include <ctime>    
#include <vector>
#include <queue>
#include <bitset>
#define PROGRAM_SIZE 65536
#define STACK_SIZE 10000000
#define HEAP_SIZE 10000000
#define NEXT_INSTRUCTION \
goto *(void *)(label_tab[*PC]);	

using namespace std;

enum OPCODES {
	HALT = 0x00,JUMP = 0x01,JNZ = 0x02,DUP = 0x03,SWAP = 0x04,DROP = 0x05,PUSH4 = 0x06, PUSH2 = 0x07, PUSH1 = 0x08,	ADD = 0x09,	SUB = 0x0a,	MUL = 0x0b,	DIV = 0x0c,
	MOD = 0x0d,	EQ = 0x0e, NE = 0x0f, LT = 0x10, GT = 0x11,	LE = 0x12, GE = 0x13, NOT = 0x14, AND = 0x15, OR = 0x16, INPUT = 0x17, OUTPUT = 0x18, CLOCK = 0x2a,
    CONS = 0x30, HD = 0x31, TL = 0x32
};

unsigned char program[PROGRAM_SIZE];
int stack[STACK_SIZE];
int size = -1;
struct cons_cell {
    int head;
    int tail;
    int mark:1;
};
struct cons_cell heap[HEAP_SIZE];
int free_list[HEAP_SIZE];
int free_size = -1;

void DFS(int x){
    if (x&1){
        x = x>>1;
        if (!heap[x].mark){
            heap[x].mark = 1;
            DFS(heap[x].head);
            DFS(heap[x].tail);
        }
    }
}

void Sweep(){
    for (int p = 0; p < HEAP_SIZE; p++) {
        if (heap[p].mark)
        {
            heap[p].mark = 0;
        }
        else
        {
        	free_size++;
            free_list[free_size] = p;
        }
    }
}

void MarkandSweepGC() {
  for(int i=0; i<=size; i++) DFS(stack[i]);
  Sweep();
}

int main (int argc, char *argv[]) {

	static void *label_tab[0x32+1]; //An array containing the labels of the operations.
	
	label_tab[0x00] = &&halt; 
	label_tab[0x01] = &&jump; 
	label_tab[0x02] = &&jnz; 
	label_tab[0x03] = &&dup; 
	label_tab[0x04] = &&swap; 
	label_tab[0x05] = &&drop; 
	label_tab[0x06] = &&push4; 
	label_tab[0x07] = &&push2; 
	label_tab[0x08] = &&push1; 
	label_tab[0x09] = &&add; 
	label_tab[0x0a] = &&sub; 
	label_tab[0x0b] = &&mul; 
	label_tab[0x0c] = &&div; 
	label_tab[0x0d] = &&mod; 
	label_tab[0x0e] = &&eq; 
	label_tab[0x0f] = &&ne; 
	label_tab[0x10] = &&lt; 
	label_tab[0x11] = &&gt;	
	label_tab[0x12] = &&le;	
	label_tab[0x13] = &&ge;	
	label_tab[0x14] = &&not_; 
	label_tab[0x15] = &&and_; 
	label_tab[0x16] = &&or_; 
	label_tab[0x17] = &&input; 
	label_tab[0x18] = &&output; 
	label_tab[0x2a] = &&clock;
    label_tab[0x30] = &&cons;
    label_tab[0x31] = &&hd;
    label_tab[0x32] = &&tl;

    for (int i = 0; i < HEAP_SIZE; i++) {
    	free_size++;
        free_list[free_size] = i;
    }

    FILE *fp = NULL; // Try to open the file containing the program.
	if((fp = fopen(argv[1], "r")) == NULL) 
	{
		perror("Can't open file.");
		exit(1);
	}

	unsigned char hex; // Read and store the program.
	int i = 0;
    while (fscanf(fp, "%c", &hex) == 1) { 
    	program[i++] = hex;
    }
    program[i] = EOF;

    fclose(fp); // Close the file.

    unsigned char OP, *PC = program; // Create a Program Counter pointing at the start of the program array.
    int a,b,address;
    float run_time;
    cons_cell cons;
    clock_t start = clock(); // Record the timestamp when the program started. 
    while (*PC != EOF) { 
    	OP = PC[0];
    	switch (OP) {
    		case HALT: // Terminates the Stack Machine.
    		halt:
    			return 0;
    		case JUMP: // Jumps to the address given by the next 2 bytes.
    		jump:	
    			PC = &program[PC[1]+(PC[2] << 8)];
    			NEXT_INSTRUCTION;
    		case JNZ: // Removes the element at the top of the stack and, if it's not zero, jumps to the address given by the next 2 bytes.
    		jnz:
    			if (stack[size]>>1 != 0) {
    				PC = &program[PC[1]+(PC[2] << 8)];
    			}
    			else {
    				PC += 3;
    			}
    			size--;
    			NEXT_INSTRUCTION;
    		case DUP: // Inserts at the top of the stack a copy of the i-th element of the stack
    		dup:
    			size++;
    			stack[size] = (stack[size-1-PC[1]]);
    			PC += 2;
    			NEXT_INSTRUCTION;
    		case SWAP: // Swaps the element at the top of the stack with the i-th element of the stack.
    		swap:
    			swap(stack[size-PC[1]],stack[size]);
	    		PC += 2;
	    		NEXT_INSTRUCTION;
    		case DROP: // Removes the element at the top of the stack.
    		drop:
    			size--;
    			PC++;
    			NEXT_INSTRUCTION;
    		case PUSH4: // Inserts at the top of the stack a 4 byte number.
    		push4:
    			size++;
    			stack[size] = ((PC[1]+(PC[2] << 8)+(PC[3] << 16)+(PC[4] << 24))<<1);
    			PC += 5;
    			NEXT_INSTRUCTION;
    		case PUSH2: // Inserts at the top of the stack a 2 byte number.
    		push2:
    			size++;
    			stack[size] = ((PC[1]+(PC[2] << 8))<<1);
    			PC += 3;
    			NEXT_INSTRUCTION;
    		case PUSH1: // Inserts at the top of the stack a 1 byte number.
    		push1:
    			size++;
    			stack[size] = (PC[1]<<1);
    			PC += 2;
    			NEXT_INSTRUCTION;
    		case ADD: // Removes in order at the top of the stack 2 elements, b and a, and inserts at the top of the stack the sum a+b.
    		add:
				b = stack[size]>>1;
    			size--;
    			a = stack[size]>>1;
    			stack[size] = (a+b)<<1;
    			PC++;
    			NEXT_INSTRUCTION;
    		case SUB: // Removes in order at the top of the stack 2 elements, b and a, and inserts at the top of the stack the difference a-b.
    		sub:
    			b = stack[size]>>1;
    			size--;
    			a = stack[size]>>1;
    			stack[size] = (a-b)<<1;
    			PC++;
    			NEXT_INSTRUCTION;
    		case MUL: // Removes in order at the top of the stack 2 elements, b and a, and inserts at the top of the stack the product a*b.
    		mul:
				b = stack[size]>>1;
    			size--;
    			a = stack[size]>>1;
    			stack[size] = (a*b)<<1;
    			PC++;
    			NEXT_INSTRUCTION;
    		case DIV: // Removes in order at the top of the stack 2 elements, b and a, and inserts at the top of the stack the quotient of the integer division a/b.
    		div:
				b = stack[size]>>1;
    			size--;
    			a = stack[size]>>1;
    			stack[size] = (a/b)<<1;
    			PC++;
    			NEXT_INSTRUCTION;
    		case MOD: // Removes in order at the top of the stack 2 elements, b and a, and inserts at the top of the stack the remainder of the integer division a/b.
    		mod:
    			b = stack[size]>>1;
    			size--;
    			a = stack[size]>>1;
    			stack[size] = (a%b)<<1;
    			PC++;
    			NEXT_INSTRUCTION;
    		case EQ: // Removes in order at the top of the stack 2 elements, b and a, and inserts at the top of the stack the number 1, if a=b, else the number 0.
    		eq:
				b = stack[size]>>1;
    			size--;
    			a = stack[size]>>1;
    			stack[size] = (a==b)<<1;
    			PC++;
    			NEXT_INSTRUCTION;
    		case NE: // Removes in order at the top of the stack 2 elements, b and a, and inserts at the top of the stack the number 1, if a!=b, else the number 0.
    		ne:
				b = stack[size]>>1;
    			size--;
    			a = stack[size]>>1;
    			stack[size] = (a!=b)<<1;
    			PC++;
    			NEXT_INSTRUCTION;
    		case LT: // Removes in order at the top of the stack 2 elements, b and a, and inserts at the top of the stack the number 1, if a<b, else the number 0.
    		lt:
				b = stack[size]>>1;
				size--;
				a = stack[size]>>1;
				stack[size] = (a<b)<<1;
				PC++;
				NEXT_INSTRUCTION;
    		case GT: // Removes in order at the top of the stack 2 elements, b and a, and inserts at the top of the stack the number 1, if a>b, else the number 0.
    		gt:
				b = stack[size]>>1;
    			size--;
    			a = stack[size]>>1;
    			stack[size] = (a>b)<<1;
    			PC++;
    			NEXT_INSTRUCTION;
    		case LE: // Removes in order at the top of the stack 2 elements, b and a, and inserts at the top of the stack the number 1, if a<=b, else the number 0.
	    	le:
    			b = stack[size]>>1;
    			size--;
    			a = stack[size]>>1;
    			stack[size] = (a<=b)<<1;
    			PC++;
    			NEXT_INSTRUCTION;
    		case GE: // Removes in order at the top of the stack 2 elements, b and a, and inserts at the top of the stack the number 1, if a>=b, else the number 0.
    		ge:
				b = stack[size]>>1;
    			size--;
    			a = stack[size]>>1;
    			stack[size] = (a>=b)<<1;
    			PC++;
    			NEXT_INSTRUCTION;
    		case NOT: // Removes the element at the top of the stack and inserts at the top of the stack the number 1, if it was 0, else the number 0.
    		not_:
    			if ((stack[size]>>1) == 0)
    			{
    				stack[size] <<= 1;
    			}
    			else
    			{ 
    				stack[size] = 0;
    			}
    			PC++;
    			NEXT_INSTRUCTION;
    		case AND: // Removes at the top of the stack the first 2 elements and inserts at the top of the stack the number 1, if both were not zero, else the number 0.
	    	and_:
    			b = stack[size]>>1;
    			size--;
    			a = stack[size]>>1;
    			if (a!=0 && b!=0){
    				stack[size] <<= 1;
    			}
    			else {
    				stack[size] = 0;
    			}
    			PC++;
    			NEXT_INSTRUCTION;
    		case OR: // Removes at the top of the stack the first 2 elements and inserts at the top of the stack the number 1, if both were zero, else the number 0.
    		or_:
				b = stack[size]>>1;
    			size--;
    			a = stack[size]>>1;
    			if (a==0 && b==0) {
    				stack[size] = 0;
    			}
    			else {
    				stack[size] <<= 1;
    			}
    			PC++;
    			NEXT_INSTRUCTION;
    		case INPUT: // Reads an input character and inserts at the top of the stack its ASCII code.
    		input:
    			stack[size] = getchar()<<1;
    			PC++;
    			NEXT_INSTRUCTION;
    		case OUTPUT: // Removes the element at the top of the stack and prints the character corresponding to that ASCII code.
    		output:
    			putchar(stack[size]>>1);
    			size--;
    			PC++;
    			NEXT_INSTRUCTION;
    		case CLOCK: // Prints at the standard input the number of seconds that have passed since the start of the program execution, with 6 decimal digits.
    		clock:
				run_time = (clock() - start)/(float) CLOCKS_PER_SEC;
				printf("%0.6lf\n", run_time);
				PC++;
				NEXT_INSTRUCTION;
            case CONS:
            cons:
                b = stack[size];
                size--;
                a = stack[size];
                if (free_size==-1){
                    MarkandSweepGC();
                    if (free_size==-1) {printf("Heap Full"); exit(1);}
                } 
                address = free_list[free_size];
                free_size--;
                heap[address].head = a;
                heap[address].tail = b;
                heap[address].mark = 0;
                address = (address << 1)+1; 
                stack[size] = address;
                PC++;
                NEXT_INSTRUCTION;
            case HD:
            hd:
                cons = heap[stack[size] >> 1];
                stack[size] = cons.head; 
                PC++;
                NEXT_INSTRUCTION;
            case TL:
            tl:
                cons = heap[stack[size] >> 1];
                stack[size] = cons.tail; 
                PC++;
                NEXT_INSTRUCTION;
    		default: perror("OP code not recognised.");
    	}
    }
    return 0;
}