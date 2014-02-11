proper-examples
===============

### Compile the NIF
 
    $ gcc -I /usr/lib/erlang/usr/include -o q_nif.so -fpic -shared  q_nif.c

### Compile q.erl and q_proper.erl

    $ erlc q.erl q_proper.erl
    
### Run the tests

    $ erl
    1> proper:quickcheck(q_proper:prop_queue_behaviour()).
