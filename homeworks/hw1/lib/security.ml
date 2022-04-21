
type ('env,'action) sandbox ={
  permissions: 'action list
; external_env: 'env
; internal_env: 'env
}


let allowed (a:'action) (sandbox: ('env,'action) sandbox) = List.mem a sandbox.permissions




(*
sandbox idea:
- analyze expr and get list of security relevant actions
- iterate over actions and check that actions allowed 
- recursive execute: allow in special cases? evaluate other execute with other permissions added? remove execute arrived?
- more complex policies: reuse automata? PCC? Types?  

 *)


