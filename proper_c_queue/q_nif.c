#include "erl_nif.h"

typedef struct queue
{
  int inp;  /* input pointer */
  int outp; /* output pointer */
  int size; /* size of the buffer */
  int *buf;
} Queue;


ErlNifResourceType* RES_TYPE;
ERL_NIF_TERM atom_ok;

static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
  //const char* mod = "resources"; 
  const char* name = "queue_type";
  int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;

    RES_TYPE = enif_open_resource_type(env, NULL, name, NULL, flags, NULL);
    if(RES_TYPE == NULL) return -1;    
    atom_ok = enif_make_atom(env, "ok");
    return 0;
}

static ERL_NIF_TERM new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    Queue* qptr;
    int *buff;
    ERL_NIF_TERM ret;
    int n;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_int(env, argv[0], &n))
    {
        return enif_make_badarg(env);
    }

    buff = malloc((n+1)*sizeof(int));
    Queue q = {0, 0, n+1, buff};
    qptr = enif_alloc_resource(RES_TYPE, sizeof(Queue));
    if(qptr == NULL) return enif_make_badarg(env);

    *qptr = q;
    ret = enif_make_resource(env, qptr);
    enif_release_resource(qptr);
    return ret;
}


static ERL_NIF_TERM put(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    Queue* qptr;
    int n;

    if(argc != 2)
    {
      return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void**) &qptr))
    {
      return enif_make_badarg(env);
    }

    if(!enif_get_int(env, argv[1], &n))
    {
        return enif_make_badarg(env);
    }

    qptr->buf[qptr->inp] = n;
    qptr->inp = (qptr->inp + 1) % qptr->size; 
    return enif_make_int(env, n);
}


static ERL_NIF_TERM get(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    Queue* qptr;
    int x;

    if(argc != 1)
    {
      return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void**) &qptr))
    {
      return enif_make_badarg(env);
    }

    x = qptr->buf[qptr->outp];
    qptr->outp = (qptr->outp + 1) % qptr->size;
    return enif_make_int(env, x);
}

static ERL_NIF_TERM size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    Queue* qptr;
    int size;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void**) &qptr))
    {
      return enif_make_badarg(env);
    }

    size = (qptr->inp - qptr->outp + qptr->size) % qptr->size; 
    return enif_make_int(env, size);
}

static ErlNifFunc nif_funcs[] = {
    {"get", 1, get},
    {"size", 1, size},
    {"new", 1, new},
    {"put", 2, put}
};

ERL_NIF_INIT(q, nif_funcs, &load, NULL, NULL, NULL);
