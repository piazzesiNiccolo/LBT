
type  ('event, 'code) sandbox = {
 code: 'code
 ; permissions:  'event list
}


(*
sandbox idea:
- analyze expr and get list of security relevant actions
- iterate over actions and check that actions allowed 
- recursive execute: allow in special cases? evaluate other execute with other permissions added? remove execute arrived?
- more complex policies: reuse automata? PCC? Types?

 *)