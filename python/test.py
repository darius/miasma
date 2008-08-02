import x86
from x86 import eax, ebx, esi, edi, ebp, esp
from x86 import atv, reg

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

def emit_add42():
    x86.ret()
    epilog()
    x86.add_gv_ev(eax, atv(ebp, 8))
    x86.mov_gv_iv(eax, 42)
    prolog()


def dump_code():
    dump_hex(x86.freeze_code_buffer())

def dump_hex(str):
    for b in str:
        print '%02x' % ord(b),
    print


import shoothead

def test():
    emit_add42()
    add42 = x86.freeze_code_buffer()
    print shoothead.call(add42, 8)   # should print 50 (= 42+8)
