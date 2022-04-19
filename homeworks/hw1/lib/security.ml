



type proof = int

type  ('event, 'code) sandbox = {
 code: 'code
 ; permissions:  'event list
}


type policy = int 
type checker = {

  policies: policy list
}