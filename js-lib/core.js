/* ~ ~ ~ ~ ~ (begin core library) ~ ~ ~ ~ ~ */

class SchemeObject {
    type = "object";

    constructor(val) {
        this.val = val;
    }

    truthy() {
        return true;
    }

    call() {
        console.error("application: not a procedure: ", this);
    }

    and(other) {
        if(!this.truthy()) return this;
        else return other;
    }

    or(other) {
        if(this.truthy()) return this;
        else return other;
    }
}

class SchemeBool extends SchemeObject {
    type = "bool";

    truthy() {
        return this.val;
    }
}

class SchemeNil extends SchemeObject {
    type = "nil";

    truthy() {
        return false;
    }

    constructor() {
        super(null);
    }
}

class SchemeProcedure extends SchemeObject {
    type = "procedure";

    constructor(num_args, is_variadic, f) {
        super(f);
        this.num_args = num_args;
        this.is_variadic = is_variadic;
    }

    call(...args) {
        if(this.is_variadic && args.length >= this.num_args - 1) {
            let l = new SchemeNil();
            while(args.length >= this.num_args) l = new SchemePair({car: args.pop(), cdr: l});
            args.push(l);
        }

        if(args.length != this.num_args) {
            console.error("invalid args supplied to procedure: ", this.val, "(got ", args.length, "expected", this.num_args, ")");
            return undefined;
        }

        return this.val(...args);
    }
}

class SchemePair extends SchemeObject {
    type = "pair";
}

class SchemeNum extends SchemeObject { type = "number"; }
class SchemeChar extends SchemeObject { type = "char"; }
class SchemeString extends SchemeObject { type = "string"; }
class SchemeSymbol extends SchemeObject { type = "symbol"; }
class SchemeVector extends SchemeObject { type = "vector"; }

/* helpers */

function err(message) {
    console.error(message);
    return new SchemeNil();
}

function list_to_vec(l) {
    // assume l is list
    let v = [];

    while(l.type != "nil") {
        v.push(l.val.car);
        l = l.val.cdr;
    }

    return new SchemeVector(v);
}

function arr_to_list(a) {
    // assume v is vector
    let l = new SchemeNil();
    while(a.length) l = new SchemePair({car : a.pop(), cdr: l});
    return l;
}

function is_list(x) {
    return x.type == "nil" || (x.type == "pair" && is_list(x.val.cdr));
}

/* functions */

// misc

let s2j_eqQuestion = new SchemeProcedure(2, false, (x, y) => new SchemeBool(x.type == y.type && x.val == y.val));

let s2j_error = new SchemeProcedure(1, false, s => err(s.val));
let s2j_print = new SchemeProcedure(1, false, o => console.log(o));

// types

let s2j_booleanQuestion = new SchemeProcedure(1, false, x => new SchemeBool(x.type == "bool"));
let s2j_pairQuestion = new SchemeProcedure(1, false, x => new SchemeBool(x.type == "pair"));
let s2j_nullQuestion = new SchemeProcedure(1, false, x => new SchemeBool(x.type == "nil"));
let s2j_symbolQuestion = new SchemeProcedure(1, false, x => new SchemeBool(x.type == "symbol"));
let s2j_numberQuestion = new SchemeProcedure(1, false, x => new SchemeBool(x.type == "number"));
let s2j_stringQuestion = new SchemeProcedure(1, false, x => new SchemeBool(x.type == "string"));
let s2j_charQuestion = new SchemeProcedure(1, false, x => new SchemeBool(x.type == "char"));
let s2j_vectorQuestion = new SchemeProcedure(1, false, x => new SchemeBool(x.type == "vector"));
let s2j_procedureQuestion = new SchemeProcedure(1, false, x => new SchemeBool(x.type == "procedure"));

// conversion

let s2j_symbolSubGtstring = new SchemeProcedure(1, false, s => s.type != "symbol" ? err("symbol->string: expected symbol") : new SchemeString(s.val));
let s2j_stringSubGtSymbol = new SchemeProcedure(1, false, s => s.type != "string" ? err("string->symbol: expected string") : new SchemeSymbol(s.val));

let s2j_charSubGtinteger = new SchemeProcedure(1, false, c => c.type != "char" ? err("char->integer: expected char") : new SchemeNum(c.val.charCodeAt(0)));
let s2j_integerSubGtchar = new SchemeProcedure(1, false, i => i.type != "number" ? err("integer->char: expected integer") : new SchemeChar(String.fromCharCode(i.val)));

let s2j_stringSubGtlist = new SchemeProcedure(1, false, s => s.type != "string" ? err("string->list: expected string") : arr_to_list(s.val.split("").map(c => new SchemeChar(c))));
let s2j_listSubGtstring = new SchemeProcedure(1, false, l => !is_list(l) ? err("list->string: expected list") :
                                                             !list_to_vec(l).val.every(x => x.type == "char") ? err("list->string: expected *char* list" + list_to_vec(l).val) :
                                                             new SchemeString(list_to_vec(l).val.map(sc => sc.val).join("")));


let s2j_listSubGtvector = new SchemeProcedure(1, false, l => !is_list(l) ? err("list->vector: expected list") : list_to_vec(l));
let s2j_vectorSubGtlist = new SchemeProcedure(1, false, v => v.type != "vector" ? err("vector->list: expected vector") : arr_to_list(v.val));

// pairs

let s2j_cons = new SchemeProcedure(2, false, (x, y) => new SchemePair({car: x, cdr: y}));
let s2j_car = new SchemeProcedure(1, false, p => p.type != "pair" ? err("car: expected pair") : p.val.car);
let s2j_cdr = new SchemeProcedure(1, false, p => p.type != "pair" ? err("cdr: expected pair") : p.val.cdr);
let s2j_setSubcarBang = new SchemeProcedure(2, false, (p, v) => p.type != "pair" ? err("set-car!: expected pair") : p.val.car = v);
let s2j_setSubcdrBang = new SchemeProcedure(2, false, (p, v) => p.type != "pair" ? err("set-cdr!: expected pair") : p.val.cdr = v);

// numeric

let s2j_bAdd = new SchemeProcedure(2, false, (x, y) => x.type != "number" || y.type != "number" ? err("+b: expected numbers") : new SchemeNum(x.val + y.val))
let s2j_bSub = new SchemeProcedure(2, false, (x, y) => x.type != "number" || y.type != "number" ? err("-b: expected numbers") : new SchemeNum(x.val - y.val))
let s2j_bAst = new SchemeProcedure(2, false, (x, y) => x.type != "number" || y.type != "number" ? err("*b: expected numbers") : new SchemeNum(x.val * y.val))
let s2j_bSlash = new SchemeProcedure(2, false, (x, y) => x.type != "number" || y.type != "number" ? err("/b: expected numbers") : new SchemeNum(x.val / y.val))

let s2j_Eq = new SchemeProcedure(2, false, (x, y) => x.type != "number" || y.type != "number" ? err("=: expected numbers") : new SchemeBool(x.val == y.val));
let s2j_Lt = new SchemeProcedure(2, false, (x, y) => x.type != "number" || y.type != "number" ? err("=: expected numbers") : new SchemeBool(x.val < y.val));
let s2j_Gt = new SchemeProcedure(2, false, (x, y) => x.type != "number" || y.type != "number" ? err("=: expected numbers") : new SchemeBool(x.val > y.val));

let s2j_floor = new SchemeProcedure(1, false, x => x.type != number ? err("floor: expected number") : new SchemeNum(Math.floor(x.val)));
let s2j_ceiling = new SchemeProcedure(1, false, x => x.type != number ? err("floor: expected number") : new SchemeNum(Math.ceil(x.val)));

let s2j_exp = new SchemeProcedure(1, false, x => x.type != "number" ? err("exp: expected number") : new SchemeNum(Math.exp(x.val)));
let s2j_log = new SchemeProcedure(1, false, x => x.type != "number" ? err("log: expected number") : new SchemeNum(Math.log(x.val)));
let s2j_sin = new SchemeProcedure(1, false, x => x.type != "number" ? err("sin: expected number") : new SchemeNum(Math.sin(x.val)));
let s2j_cos = new SchemeProcedure(1, false, x => x.type != "number" ? err("cos: expected number") : new SchemeNum(Math.cos(x.val)));
let s2j_tan = new SchemeProcedure(1, false, x => x.type != "number" ? err("tan: expected number") : new SchemeNum(Math.tan(x.val)));
let s2j_asin = new SchemeProcedure(1, false, x => x.type != "number" ? err("asin: expected number") : new SchemeNum(Math.asin(x.val)));
let s2j_acos = new SchemeProcedure(1, false, x => x.type != "number" ? err("acos: expected number") : new SchemeNum(Math.acos(x.val)));
let s2j_atan = new SchemeProcedure(1, false, x => x.type != "number" ? err("atan: expected number") : new SchemeNum(Math.atan(x.val)));
let s2j_sqrt = new SchemeProcedure(1, false, x => x.type != "number" ? err("sqrt: expected number") : new SchemeNum(Math.sqrt(x.val)));
let s2j_atan2 = new SchemeProcedure(1, false, (x, y) => x.type != "number" || y.type != "number" ? err("atan2: expected numbers") : new SchemeNum(Math.atan2(x.val, y.val)));
let s2j_expt = new SchemeProcedure(1, false, (x, y) => x.type != "number" || y.type != "number" ? err("pow: expected numbers") : new SchemeNum(Math.pow(x.val, y.val)));

// mutable

// no string-set! (strings aren't mutable in js, so this doesn't work)

let s2j_vectorSubsetBang = new SchemeProcedure(3, false, (v, i, e) => v.type != "vector" ? err("vector-set!: expected vector as first arg") :
                                                                      i.type != "number" ? err("vector-set!: expected number as second arg") :
                                                                      Math.floor(i.val) != i.val ? err("vector-set!: expected integer index") :
                                                                      i.val < 0 || i.val >= v.val.length ? err("vector-set!: index out of range") :
                                                                      (() => { v.val[i.val] = e; return new SchemeNil(); })()
                                                                      );

/* ~ ~ ~ ~ ~ (end core library) ~ ~ ~ ~ ~ */
