#include <Python.h>

typedef int (*funcptr)();

static PyObject *
shoothead_call(PyObject *self, PyObject *args)
{
  const char *code_buffer;
  int length, argument, result;

  if (!PyArg_ParseTuple(args, "s#i", &code_buffer, &length, &argument))
    return NULL;
  result = ((funcptr)code_buffer)(argument);
  return Py_BuildValue("i", result);
}


static PyMethodDef ShootheadMethods[] = {
  {"call",    shoothead_call,   METH_VARARGS, "Call into machine code."},
  {NULL, NULL, 0, NULL}        /* Sentinel */
};

void
initshoothead(void)
{
  (void) Py_InitModule("shoothead", ShootheadMethods);
}
