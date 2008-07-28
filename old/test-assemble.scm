; Miasma: x86 Scheme Assembler.
; Copyright (C) 2000, Darius Bacon, Brian Spilsbury, Alycat
; Refer to legal/License.txt

(insist "Registers resolve to themselves"
	(all (lambda (r) (eq? r (resolve r '() 0))) 
	     registers))
