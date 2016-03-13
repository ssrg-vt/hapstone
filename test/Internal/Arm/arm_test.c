/* An attempt to map all struct sizes to account for the shitty C11 support
 * C2HS exposes. We just print them in a structured fashion to account for
 * the anonymous unions present in most of the code. That way, currently
 * broken Storable instances can be fixed. - This is what happens when you
 * start writing proper property-based tests ;)
 */

#include <stdio.h>

#include <capstone/arm.h>

// print all struct sizes
void print_sizes(void){
    puts("arm.h");
    printf("[*] arm_op_mem: %d\n", sizeof(arm_op_mem));
    printf("[*] cs_arm_op: %d\n", sizeof(cs_arm_op));
    printf("[*] cs_arm: %d\n", sizeof(cs_arm));
}

// print all offsets and member sizes
void print_alignment(void){
    cs_arm_op test = {0, {1, 2}, 3, 4};
    void *base = &test;

    printf("cs_arm_op: %d\n", sizeof(cs_arm_op));
    puts("cs_arm_op\toffset\tsize");
    printf("vector_index:\t%d\t%d\n", (void *)&test.vector_index - base,
            sizeof(int));
    printf("shift.type:\t%d\t%d\n", (void *)&test.shift.type - base,
            sizeof(arm_shifter));
    printf("shift.value:\t%d\t%d\n", (void *)&test.shift.value - base,
            sizeof(unsigned int));
    printf("type:\t\t%d\t%d\n", (void *)&test.type - base,
            sizeof(arm_op_type));
    printf("union.reg:\t%d\t%d\n", (void *)&test.reg - base,
            sizeof(unsigned int));
    printf("union.imm:\t%d\t%d\n", (void *)&test.imm - base,
            sizeof(int32_t));
    printf("union.fp:\t%d\t%d\n", (void *)&test.fp - base,
            sizeof(double));
    printf("union.mem:\t%d\t%d\n", (void *)&test.mem - base,
            sizeof(arm_op_mem));
    printf("union.setend:\t%d\t%d\n", (void *)&test.setend - base,
            sizeof(arm_setend_type));
    printf("subtracted:\t%d\t%d\n", (void *)&test.subtracted - base,
            sizeof(bool));
}

int main(void){
    print_sizes();
    print_alignment();

    return 0;
}
