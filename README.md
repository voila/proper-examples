proper-examples
===============

### Compile the NIF

Every time the NIF code changes
 
    $ gcc -I /usr/lib/erlang/usr/include -o q_nif.so -fpic -shared  q_nif.c

### Compile q.erl and q_proper.erl

Every time the NIF code changes

    $ erlc q.erl 

Every time the proper model changes

    $ erlc q_proper.erl
    
### Run the tests

    $ erl
    1> proper:quickcheck(q_proper:prop_queue_behaviour()).
