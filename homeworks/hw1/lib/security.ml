(*

The sandbox used to check for insecure mobile code. 
 *)

type ('env, 'action) sandbox = {
  permissions : 'action list;
  external_env : 'env;
  internal_env : 'env;
}

let allowed (a : 'action) (sandbox : ('env, 'action) sandbox) =
  List.mem a sandbox.permissions
