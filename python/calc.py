import array
import x86
from x86 import eax, ebx, ecx, edx, esi, edi, ebp, esp
from x86 import atv, reg, at_reg

def epilog():
    x86.pop_gv(ebp)
    x86.pop_gv(ebx)
    x86.pop_gv(esi)
    x86.pop_gv(edi)
    
def prolog():
    x86.push_gv(edi)
    x86.push_gv(esi)
    x86.push_gv(ebx)
    x86.mov_gv_ev(ebp, reg(esp))
    x86.push_gv(ebp)

def pop_gv(r):
    x86.mov_gv_ev (r, at_reg(esi))
    x86.sub_ev_iv (reg(esi), 4)

def push_gv(r):
    x86.add_ev_iv(reg(esi), 4);
    x86.mov_ev_gv(at_reg(esi), r);

depth = 1

def gen_push(i):
    global depth
    x86.mov_gv_iv(eax, i)
    if 0 < depth:
        push_gv(eax)
    
def emit(op):
    global depth
    # print depth, op
    if op == "+":
        x86.add_gv_ev(eax, reg(ebx))
        pop_gv(ebx)
    elif op == "-":
        x86.sub_gv_ev(eax, reg(ebx))
        pop_gv(eax)
        x86.mov_gv_ev(ebx, reg(eax))
    elif op == "*":
        x86.imul_ev(reg(ebx))
        pop_gv(ebx)
    elif op == "/":
        x86.idiv_ev(reg(ebx))
        x86.mov_gv_iv(edx, 0)
        pop_gv(eax)
        x86.mov_gv_ev(ebx, reg(eax))
    elif op == "%":
        x86.mov_gv_ev(eax, reg(edx))
        x86.idiv_ev(reg(ebx))
        x86.mov_gv_iv(edx, 0)
        pop_gv(eax)
        x86.mov_gv_ev(ebx, reg(eax))
    else:
        gen_push(op)

stack = array.array('i', [0]*256)
stack_address, stack_length = stack.buffer_info()

def compile(str):
    global depth, stack_address
    x86.ret()
    epilog()
    tokens = str.split()
    tokens.reverse()
    for token in tokens:
        try:
            op = int(token)
            depth = depth - 1
        except ValueError:
            op = token
            depth = depth + 1
        emit(op)
    x86.mov_gv_iv(esi, stack_address)
    prolog()


import shoothead

def test(str):
    compile(str)
    f = x86.freeze_code_buffer()
    # dump_hex(f)
    print shoothead.call(f, 0)


def dump_code():
    dump_hex(x86.freeze_code_buffer())

def dump_hex(str):
    for b in str:
        print '%02x' % ord(b),
    print

if __name__ == '__main__':
    import sys
    test(sys.argv[1])
