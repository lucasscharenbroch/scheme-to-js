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
            console.error("invalid args supplied to procedure: ", f, "(got ", this.num_args, "expected", args.length, ")");
        }

        return this.val(...args);
    }
}

class SchemePair extends SchemeObject {
    type = "pair";

    from_array(arr) {
        let res = new SchemeNil();
        while(arr.length > 0) res = new SchemePair({car : arr.pop(), cdr: res});
        return res;
    }
}


class SchemeNum extends SchemeObject { type = "number"; }
class SchemeChar extends SchemeObject { type = "char"; }
class SchemeString extends SchemeObject { type = "string"; }
class SchemeSymbol extends SchemeObject { type = "symbol"; }
class SchemeVector extends SchemeObject { type = "vector"; }
