/* Windows named-pipe command channel for ocsigenserver.
 *
 * On Windows, Unix.mkfifo is not available, so the command pipe (used to send
 * "shutdown", "reload", ... to a running server) cannot be created the usual
 * way. These stubs create a Windows named pipe and read commands from it. On
 * non-Windows platforms the stubs are never called (the Unix FIFO is used
 * instead); they only need to exist so the externals link. */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#ifdef _WIN32

#include <caml/threads.h>
#include <windows.h>
#include <string.h>

/* Create a byte-mode, inbound named pipe and return its HANDLE as a nativeint.
   Raises Failure on error. */
CAMLprim value ocsigen_command_pipe_create(value vname)
{
  CAMLparam1(vname);
  HANDLE h = CreateNamedPipeA(String_val(vname), PIPE_ACCESS_INBOUND,
      PIPE_TYPE_BYTE | PIPE_READMODE_BYTE | PIPE_WAIT,
      PIPE_UNLIMITED_INSTANCES, 0, 4096, 0, NULL);
  if (h == INVALID_HANDLE_VALUE) caml_failwith("CreateNamedPipe failed");
  CAMLreturn(caml_copy_nativeint((intnat)h));
}

/* Wait for a client, read one chunk of bytes, then disconnect (ready for the
   next client). Returns the bytes read as a string (empty on disconnect/error).
   The blocking calls run with the OCaml runtime lock released so the rest of
   the server keeps running; call this through Lwt_preemptive.detach. */
CAMLprim value ocsigen_command_pipe_read(value vh)
{
  CAMLparam1(vh);
  CAMLlocal1(res);
  HANDLE h = (HANDLE)Nativeint_val(vh);
  char buf[4096];
  DWORD n = 0;
  BOOL ok;
  caml_release_runtime_system();
  ok = ConnectNamedPipe(h, NULL);
  if (!ok && GetLastError() == ERROR_PIPE_CONNECTED) ok = TRUE;
  if (ok) ok = ReadFile(h, buf, (DWORD)sizeof(buf), &n, NULL);
  DisconnectNamedPipe(h);
  caml_acquire_runtime_system();
  if (!ok) n = 0;
  res = caml_alloc_string(n);
  memcpy((char *)Bytes_val(res), buf, n);
  CAMLreturn(res);
}

/* Connect to a named pipe as a client and write [data] to it. Raises Failure if
   the pipe cannot be opened. Used by the "--command" client mode. */
CAMLprim value ocsigen_command_pipe_send(value vname, value vdata)
{
  CAMLparam2(vname, vdata);
  HANDLE h = CreateFileA(String_val(vname), GENERIC_WRITE, 0, NULL,
      OPEN_EXISTING, 0, NULL);
  DWORD written = 0;
  if (h == INVALID_HANDLE_VALUE)
    caml_failwith("CreateFile (named pipe client) failed");
  WriteFile(h, String_val(vdata), (DWORD)caml_string_length(vdata), &written,
      NULL);
  CloseHandle(h);
  CAMLreturn(Val_unit);
}

#else

CAMLprim value ocsigen_command_pipe_create(value vname)
{
  caml_failwith("ocsigen_command_pipe_create: Windows only");
}

CAMLprim value ocsigen_command_pipe_read(value vh)
{
  caml_failwith("ocsigen_command_pipe_read: Windows only");
}

CAMLprim value ocsigen_command_pipe_send(value vname, value vdata)
{
  caml_failwith("ocsigen_command_pipe_send: Windows only");
}

#endif
