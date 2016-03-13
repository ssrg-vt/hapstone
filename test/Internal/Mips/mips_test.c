/* An attempt to map all struct sizes to account for the shitty C11 support
 * C2HS exposes. We just print them in a structured fashion to account for
 * the anonymous unions present in most of the code. That way, currently
 * broken Storable instances can be fixed. - This is what happens when you
 * start writing proper property-based tests ;)
 */

#include <stdio.h>

#include <capstone/mips.h>

// print all struct sizes
void print_sizes(void){
    puts("mips.h");
    printf("[*] mips_op_mem: %d\n", sizeof(mips_op_mem));
    printf("[*] cs_mips_op: %d\n", sizeof(cs_mips_op));
    printf("[*] cs_mips: %d\n", sizeof(cs_mips));
}

// print all offsets and member sizes
void print_alignment(void){
    cs_mips_op test = {0, 1};
    void *base = &test;

    printf("cs_mips_op: %d\n", sizeof(cs_mips_op));
    puts("cs_mips_op\toffset\tsize");
    printf("type:\t\t%d\t%d\n", (void *)&test.type - base,
            sizeof(mips_op_type));
    printf("union.reg:\t%d\t%d\n", (void *)&test.reg - base,
            sizeof(unsigned int));
    printf("union.imm:\t%d\t%d\n", (void *)&test.imm - base,
            sizeof(int64_t));
    printf("union.mem:\t%d\t%d\n", (void *)&test.mem - base,
            sizeof(mips_op_mem));
}

int main(void){
    print_sizes();
    print_alignment();

    return 0;
}
