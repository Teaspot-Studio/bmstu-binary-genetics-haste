// This object will hold all exports.
var Haste = {};

/* Thunk
   Creates a thunk representing the given closure.
   If the non-updatable flag is undefined, the thunk is updatable.
*/
function T(f, nu) {
    this.f = f;
    if(nu === undefined) {
        this.x = __updatable;
    }
}

function F(f) {
    this.f = f;
}

// Special object used for blackholing.
var __blackhole = {};

// Used to indicate that an object is updatable.
var __updatable = {};

/* Apply
   Applies the function f to the arguments args. If the application is under-
   saturated, a closure is returned, awaiting further arguments. If it is over-
   saturated, the function is fully applied, and the result (assumed to be a
   function) is then applied to the remaining arguments.
*/
function A(f, args) {
    if(f instanceof T) {
        f = E(f);
    }
    // Closure does some funny stuff with functions that occasionally
    // results in non-functions getting applied, so we have to deal with
    // it.
    if(!(f instanceof Function)) {
        f = B(f);
        if(!(f instanceof Function)) {
            return f;
        }
    }

    if(f.arity === undefined) {
        f.arity = f.length;
    }
    if(args.length === f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return f(args[0]);
            default: return f.apply(null, args);
        }
    } else if(args.length > f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return A(f(args.shift()), args);
            default: return A(f.apply(null, args.splice(0, f.arity)), args);
        }
    } else {
        var g = function() {
            return A(f, args.concat(Array.prototype.slice.call(arguments)));
        };
        g.arity = f.arity - args.length;
        return g;
    }
}

/* Eval
   Evaluate the given thunk t into head normal form.
   If the "thunk" we get isn't actually a thunk, just return it.
*/
function E(t) {
    if(t instanceof T) {
        if(t.f !== __blackhole) {
            var f = t.f;
            t.f = __blackhole;
            if(t.x === __updatable) {
                t.x = f();
            } else {
                return f();
            }
        }
        return t.x;
    } else {
        return t;
    }
}

/* Bounce
   Bounce on a trampoline for as long as we get a function back.
*/
function B(f) {
    while(f instanceof F) {
        var fun = f.f;
        f.f = __blackhole;
        f = fun();
    }
    return f;
}

// Export Haste, A, B and E. Haste because we need to preserve exports, A, B
// and E because they're handy for Haste.Foreign.
if(!window) {
    var window = {};
}
window['Haste'] = Haste;
window['A'] = A;
window['E'] = E;
window['B'] = B;


/* Throw an error.
   We need to be able to use throw as an exception so we wrap it in a function.
*/
function die(err) {
    throw err;
}

function quot(a, b) {
    return (a-a%b)/b;
}

function quotRemI(a, b) {
    return [0, (a-a%b)/b, a%b];
}

// 32 bit integer multiplication, with correct overflow behavior
// note that |0 or >>>0 needs to be applied to the result, for int and word
// respectively.
if(Math.imul) {
    var imul = Math.imul;
} else {
    var imul = function(a, b) {
        // ignore high a * high a as the result will always be truncated
        var lows = (a & 0xffff) * (b & 0xffff); // low a * low b
        var aB = (a & 0xffff) * (b & 0xffff0000); // low a * high b
        var bA = (a & 0xffff0000) * (b & 0xffff); // low b * high a
        return lows + aB + bA; // sum will not exceed 52 bits, so it's safe
    }
}

function addC(a, b) {
    var x = a+b;
    return [0, x & 0xffffffff, x > 0x7fffffff];
}

function subC(a, b) {
    var x = a-b;
    return [0, x & 0xffffffff, x < -2147483648];
}

function sinh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / 2;
}

function tanh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / (Math.exp(arg) + Math.exp(-arg));
}

function cosh (arg) {
    return (Math.exp(arg) + Math.exp(-arg)) / 2;
}

// Scratch space for byte arrays.
var rts_scratchBuf = new ArrayBuffer(8);
var rts_scratchW32 = new Uint32Array(rts_scratchBuf);
var rts_scratchFloat = new Float32Array(rts_scratchBuf);
var rts_scratchDouble = new Float64Array(rts_scratchBuf);

function decodeFloat(x) {
    rts_scratchFloat[0] = x;
    var sign = x < 0 ? -1 : 1;
    var exp = ((rts_scratchW32[0] >> 23) & 0xff) - 150;
    var man = rts_scratchW32[0] & 0x7fffff;
    if(exp === 0) {
        ++exp;
    } else {
        man |= (1 << 23);
    }
    return [0, sign*man, exp];
}

function decodeDouble(x) {
    rts_scratchDouble[0] = x;
    var sign = x < 0 ? -1 : 1;
    var manHigh = rts_scratchW32[1] & 0xfffff;
    var manLow = rts_scratchW32[0];
    var exp = ((rts_scratchW32[1] >> 20) & 0x7ff) - 1075;
    if(exp === 0) {
        ++exp;
    } else {
        manHigh |= (1 << 20);
    }
    return [0, sign, manHigh, manLow, exp];
}

function isFloatFinite(x) {
    return isFinite(x);
}

function isDoubleFinite(x) {
    return isFinite(x);
}

function err(str) {
    die(toJSStr(str));
}

/* unpackCString#
   NOTE: update constructor tags if the code generator starts munging them.
*/
function unCStr(str) {return unAppCStr(str, [0]);}

function unFoldrCStr(str, f, z) {
    var acc = z;
    for(var i = str.length-1; i >= 0; --i) {
        acc = B(A(f, [[0, str.charCodeAt(i)], acc]));
    }
    return acc;
}

function unAppCStr(str, chrs) {
    var i = arguments[2] ? arguments[2] : 0;
    if(i >= str.length) {
        return E(chrs);
    } else {
        return [1,[0,str.charCodeAt(i)],new T(function() {
            return unAppCStr(str,chrs,i+1);
        })];
    }
}

function charCodeAt(str, i) {return str.charCodeAt(i);}

function fromJSStr(str) {
    return unCStr(E(str));
}

function toJSStr(hsstr) {
    var s = '';
    for(var str = E(hsstr); str[0] == 1; str = E(str[2])) {
        s += String.fromCharCode(E(str[1])[1]);
    }
    return s;
}

// newMutVar
function nMV(val) {
    return ({x: val});
}

// readMutVar
function rMV(mv) {
    return mv.x;
}

// writeMutVar
function wMV(mv, val) {
    mv.x = val;
}

// atomicModifyMutVar
function mMV(mv, f) {
    var x = B(A(f, [mv.x]));
    mv.x = x[1];
    return x[2];
}

function localeEncoding() {
    var le = newByteArr(5);
    le['v']['i8'][0] = 'U'.charCodeAt(0);
    le['v']['i8'][1] = 'T'.charCodeAt(0);
    le['v']['i8'][2] = 'F'.charCodeAt(0);
    le['v']['i8'][3] = '-'.charCodeAt(0);
    le['v']['i8'][4] = '8'.charCodeAt(0);
    return le;
}

var isDoubleNaN = isNaN;
var isFloatNaN = isNaN;

function isDoubleInfinite(d) {
    return (d === Infinity);
}
var isFloatInfinite = isDoubleInfinite;

function isDoubleNegativeZero(x) {
    return (x===0 && (1/x)===-Infinity);
}
var isFloatNegativeZero = isDoubleNegativeZero;

function strEq(a, b) {
    return a == b;
}

function strOrd(a, b) {
    if(a < b) {
        return [0];
    } else if(a == b) {
        return [1];
    }
    return [2];
}

function jsCatch(act, handler) {
    try {
        return B(A(act,[0]));
    } catch(e) {
        return B(A(handler,[e, 0]));
    }
}

/* Haste represents constructors internally using 1 for the first constructor,
   2 for the second, etc.
   However, dataToTag should use 0, 1, 2, etc. Also, booleans might be unboxed.
 */
function dataToTag(x) {
    if(x instanceof Array) {
        return x[0];
    } else {
        return x;
    }
}

function __word_encodeDouble(d, e) {
    return d * Math.pow(2,e);
}

var __word_encodeFloat = __word_encodeDouble;
var jsRound = Math.round;
var jsTrunc = Math.trunc ? Math.trunc : function(x) {
    return x < 0 ? Math.ceil(x) : Math.floor(x);
};
function jsRoundW(n) {
    return Math.abs(jsTrunc(n));
}
var realWorld = undefined;
if(typeof _ == 'undefined') {
    var _ = undefined;
}

function popCnt(i) {
    i = i - ((i >> 1) & 0x55555555);
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
    return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
}

function jsAlert(val) {
    if(typeof alert != 'undefined') {
        alert(val);
    } else {
        print(val);
    }
}

function jsLog(val) {
    console.log(val);
}

function jsPrompt(str) {
    var val;
    if(typeof prompt != 'undefined') {
        val = prompt(str);
    } else {
        print(str);
        val = readline();
    }
    return val == undefined ? '' : val.toString();
}

function jsEval(str) {
    var x = eval(str);
    return x == undefined ? '' : x.toString();
}

function isNull(obj) {
    return obj === null;
}

function jsRead(str) {
    return Number(str);
}

function jsShowI(val) {return val.toString();}
function jsShow(val) {
    var ret = val.toString();
    return val == Math.round(val) ? ret + '.0' : ret;
}

function jsGetMouseCoords(e) {
    var posx = 0;
    var posy = 0;
    if (!e) var e = window.event;
    if (e.pageX || e.pageY) 	{
	posx = e.pageX;
	posy = e.pageY;
    }
    else if (e.clientX || e.clientY) 	{
	posx = e.clientX + document.body.scrollLeft
	    + document.documentElement.scrollLeft;
	posy = e.clientY + document.body.scrollTop
	    + document.documentElement.scrollTop;
    }
    return [posx - (e.currentTarget.offsetLeft || 0),
	    posy - (e.currentTarget.offsetTop || 0)];
}

function jsSetCB(elem, evt, cb) {
    // Count return press in single line text box as a change event.
    if(evt == 'change' && elem.type.toLowerCase() == 'text') {
        setCB(elem, 'keyup', function(k) {
            if(k == '\n'.charCodeAt(0)) {
                B(A(cb,[[0,k.keyCode],0]));
            }
        });
    }

    var fun;
    switch(evt) {
    case 'click':
    case 'dblclick':
    case 'mouseup':
    case 'mousedown':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            B(A(cb,[[0,x.button],[0,mx,my],0]));
        };
        break;
    case 'mousemove':
    case 'mouseover':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            B(A(cb,[[0,mx,my],0]));
        };
        break;
    case 'keypress':
    case 'keyup':
    case 'keydown':
        fun = function(x) {B(A(cb,[[0,x.keyCode],0]));};
        break;
    case 'wheel':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            var mdx = [0,x.deltaX];
            var mdy = [0,x.deltaY];
            var mdz = [0,x.deltaZ];
            B(A(cb,[[0,mx,my],[0,mdx,mdy,mdz],0]));
        };
        break;
    default:
        fun = function() {B(A(cb,[0]));};
        break;
    }
    return setCB(elem, evt, fun);
}

function setCB(elem, evt, cb) {
    if(elem.addEventListener) {
        elem.addEventListener(evt, cb, false);
        return true;
    } else if(elem.attachEvent) {
        elem.attachEvent('on'+evt, cb);
        return true;
    }
    return false;
}

function jsSetTimeout(msecs, cb) {
    window.setTimeout(function() {B(A(cb,[0]));}, msecs);
}

function jsGet(elem, prop) {
    return elem[prop].toString();
}

function jsSet(elem, prop, val) {
    elem[prop] = val;
}

function jsGetAttr(elem, prop) {
    if(elem.hasAttribute(prop)) {
        return elem.getAttribute(prop).toString();
    } else {
        return "";
    }
}

function jsSetAttr(elem, prop, val) {
    elem.setAttribute(prop, val);
}

function jsGetStyle(elem, prop) {
    return elem.style[prop].toString();
}

function jsSetStyle(elem, prop, val) {
    elem.style[prop] = val;
}

function jsKillChild(child, parent) {
    parent.removeChild(child);
}

function jsClearChildren(elem) {
    while(elem.hasChildNodes()){
        elem.removeChild(elem.lastChild);
    }
}

function jsFind(elem) {
    var e = document.getElementById(elem)
    if(e) {
        return [1,[0,e]];
    }
    return [0];
}

function jsElemsByClassName(cls) {
    var es = document.getElementsByClassName(cls);
    var els = [0];

    for (var i = es.length-1; i >= 0; --i) {
        els = [1, [0, es[i]], els];
    }
    return els;
}

function jsQuerySelectorAll(elem, query) {
    var els = [0], nl;

    if (!elem || typeof elem.querySelectorAll !== 'function') {
        return els;
    }

    nl = elem.querySelectorAll(query);

    for (var i = nl.length-1; i >= 0; --i) {
        els = [1, [0, nl[i]], els];
    }

    return els;
}

function jsCreateElem(tag) {
    return document.createElement(tag);
}

function jsCreateTextNode(str) {
    return document.createTextNode(str);
}

function jsGetChildBefore(elem) {
    elem = elem.previousSibling;
    while(elem) {
        if(typeof elem.tagName != 'undefined') {
            return [1,[0,elem]];
        }
        elem = elem.previousSibling;
    }
    return [0];
}

function jsGetLastChild(elem) {
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetFirstChild(elem) {
    var len = elem.childNodes.length;
    for(var i = 0; i < len; i++) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetChildren(elem) {
    var children = [0];
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            children = [1, [0,elem.childNodes[i]], children];
        }
    }
    return children;
}

function jsSetChildren(elem, children) {
    children = E(children);
    jsClearChildren(elem, 0);
    while(children[0] === 1) {
        elem.appendChild(E(E(children[1])[1]));
        children = E(children[2]);
    }
}

function jsAppendChild(child, container) {
    container.appendChild(child);
}

function jsAddChildBefore(child, container, after) {
    container.insertBefore(child, after);
}

var jsRand = Math.random;

// Concatenate a Haskell list of JS strings
function jsCat(strs, sep) {
    var arr = [];
    strs = E(strs);
    while(strs[0]) {
        strs = E(strs);
        arr.push(E(strs[1])[1]);
        strs = E(strs[2]);
    }
    return arr.join(sep);
}

var jsJSONParse = JSON.parse;

// JSON stringify a string
function jsStringify(str) {
    return JSON.stringify(str);
}

// Parse a JSON message into a Haste.JSON.JSON value.
// As this pokes around inside Haskell values, it'll need to be updated if:
// * Haste.JSON.JSON changes;
// * E() starts to choke on non-thunks;
// * data constructor code generation changes; or
// * Just and Nothing change tags.
function jsParseJSON(str) {
    try {
        var js = JSON.parse(str);
        var hs = toHS(js);
    } catch(_) {
        return [0];
    }
    return [1,hs];
}

function toHS(obj) {
    switch(typeof obj) {
    case 'number':
        return [0, jsRead(obj)];
    case 'string':
        return [1, obj];
    case 'boolean':
        return [2, obj]; // Booleans are special wrt constructor tags!
    case 'object':
        if(obj instanceof Array) {
            return [3, arr2lst_json(obj, 0)];
        } else if (obj == null) {
            return [5];
        } else {
            // Object type but not array - it's a dictionary.
            // The RFC doesn't say anything about the ordering of keys, but
            // considering that lots of people rely on keys being "in order" as
            // defined by "the same way someone put them in at the other end,"
            // it's probably a good idea to put some cycles into meeting their
            // misguided expectations.
            var ks = [];
            for(var k in obj) {
                ks.unshift(k);
            }
            var xs = [0];
            for(var i = 0; i < ks.length; i++) {
                xs = [1, [0, [0,ks[i]], toHS(obj[ks[i]])], xs];
            }
            return [4, xs];
        }
    }
}

function arr2lst_json(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, toHS(arr[elem]), new T(function() {return arr2lst_json(arr,elem+1);}),true]
}

function arr2lst(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, arr[elem], new T(function() {return arr2lst(arr,elem+1);})]
}
window['arr2lst'] = arr2lst;

function lst2arr(xs) {
    var arr = [];
    for(; xs[0]; xs = E(xs[2])) {
        arr.push(E(xs[1]));
    }
    return arr;
}
window['lst2arr'] = lst2arr;

function ajaxReq(method, url, async, postdata, cb) {
    var xhr = new XMLHttpRequest();
    xhr.open(method, url, async);

    if(method == "POST") {
        xhr.setRequestHeader("Content-type",
                             "application/x-www-form-urlencoded");
    }
    xhr.onreadystatechange = function() {
        if(xhr.readyState == 4) {
            if(xhr.status == 200) {
                B(A(cb,[[1,[0,xhr.responseText]],0]));
            } else {
                B(A(cb,[[0],0])); // Nothing
            }
        }
    }
    xhr.send(postdata);
}

// Create a little endian ArrayBuffer representation of something.
function toABHost(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    return a;
}

function toABSwap(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    var bs = new Uint8Array(a);
    for(var i = 0, j = n-1; i < j; ++i, --j) {
        var tmp = bs[i];
        bs[i] = bs[j];
        bs[j] = tmp;
    }
    return a;
}

window['toABle'] = toABHost;
window['toABbe'] = toABSwap;

// Swap byte order if host is not little endian.
var buffer = new ArrayBuffer(2);
new DataView(buffer).setInt16(0, 256, true);
if(new Int16Array(buffer)[0] !== 256) {
    window['toABle'] = toABSwap;
    window['toABbe'] = toABHost;
}

// MVar implementation.
// Since Haste isn't concurrent, takeMVar and putMVar don't block on empty
// and full MVars respectively, but terminate the program since they would
// otherwise be blocking forever.

function newMVar() {
    return ({empty: true});
}

function tryTakeMVar(mv) {
    if(mv.empty) {
        return [0, 0, undefined];
    } else {
        var val = mv.x;
        mv.empty = true;
        mv.x = null;
        return [0, 1, val];
    }
}

function takeMVar(mv) {
    if(mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to take empty MVar!");
    }
    var val = mv.x;
    mv.empty = true;
    mv.x = null;
    return val;
}

function putMVar(mv, val) {
    if(!mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to put full MVar!");
    }
    mv.empty = false;
    mv.x = val;
}

function tryPutMVar(mv, val) {
    if(!mv.empty) {
        return 0;
    } else {
        mv.empty = false;
        mv.x = val;
        return 1;
    }
}

function sameMVar(a, b) {
    return (a == b);
}

function isEmptyMVar(mv) {
    return mv.empty ? 1 : 0;
}

// Implementation of stable names.
// Unlike native GHC, the garbage collector isn't going to move data around
// in a way that we can detect, so each object could serve as its own stable
// name if it weren't for the fact we can't turn a JS reference into an
// integer.
// So instead, each object has a unique integer attached to it, which serves
// as its stable name.

var __next_stable_name = 1;

function makeStableName(x) {
    if(!x.stableName) {
        x.stableName = __next_stable_name;
        __next_stable_name += 1;
    }
    return x.stableName;
}

function eqStableName(x, y) {
    return (x == y) ? 1 : 0;
}

var Integer = function(bits, sign) {
  this.bits_ = [];
  this.sign_ = sign;

  var top = true;
  for (var i = bits.length - 1; i >= 0; i--) {
    var val = bits[i] | 0;
    if (!top || val != sign) {
      this.bits_[i] = val;
      top = false;
    }
  }
};

Integer.IntCache_ = {};

var I_fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Integer.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Integer([value | 0], value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Integer.IntCache_[value] = obj;
  }
  return obj;
};

var I_fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Integer.ZERO;
  } else if (value < 0) {
    return I_negate(I_fromNumber(-value));
  } else {
    var bits = [];
    var pow = 1;
    for (var i = 0; value >= pow; i++) {
      bits[i] = (value / pow) | 0;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return new Integer(bits, 0);
  }
};

var I_fromBits = function(bits) {
  var high = bits[bits.length - 1];
  return new Integer(bits, high & (1 << 31) ? -1 : 0);
};

var I_fromString = function(str, opt_radix) {
  if (str.length == 0) {
    throw Error('number format error: empty string');
  }

  var radix = opt_radix || 10;
  if (radix < 2 || 36 < radix) {
    throw Error('radix out of range: ' + radix);
  }

  if (str.charAt(0) == '-') {
    return I_negate(I_fromString(str.substring(1), radix));
  } else if (str.indexOf('-') >= 0) {
    throw Error('number format error: interior "-" character');
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 8));

  var result = Integer.ZERO;
  for (var i = 0; i < str.length; i += 8) {
    var size = Math.min(8, str.length - i);
    var value = parseInt(str.substring(i, i + size), radix);
    if (size < 8) {
      var power = I_fromNumber(Math.pow(radix, size));
      result = I_add(I_mul(result, power), I_fromNumber(value));
    } else {
      result = I_mul(result, radixToPower);
      result = I_add(result, I_fromNumber(value));
    }
  }
  return result;
};


Integer.TWO_PWR_32_DBL_ = (1 << 16) * (1 << 16);
Integer.ZERO = I_fromInt(0);
Integer.ONE = I_fromInt(1);
Integer.TWO_PWR_24_ = I_fromInt(1 << 24);

var I_toInt = function(self) {
  return self.bits_.length > 0 ? self.bits_[0] : self.sign_;
};

var I_toWord = function(self) {
  return I_toInt(self) >>> 0;
};

var I_toNumber = function(self) {
  if (isNegative(self)) {
    return -I_toNumber(I_negate(self));
  } else {
    var val = 0;
    var pow = 1;
    for (var i = 0; i < self.bits_.length; i++) {
      val += I_getBitsUnsigned(self, i) * pow;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return val;
  }
};

var I_getBits = function(self, index) {
  if (index < 0) {
    return 0;
  } else if (index < self.bits_.length) {
    return self.bits_[index];
  } else {
    return self.sign_;
  }
};

var I_getBitsUnsigned = function(self, index) {
  var val = I_getBits(self, index);
  return val >= 0 ? val : Integer.TWO_PWR_32_DBL_ + val;
};

var getSign = function(self) {
  return self.sign_;
};

var isZero = function(self) {
  if (self.sign_ != 0) {
    return false;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    if (self.bits_[i] != 0) {
      return false;
    }
  }
  return true;
};

var isNegative = function(self) {
  return self.sign_ == -1;
};

var isOdd = function(self) {
  return (self.bits_.length == 0) && (self.sign_ == -1) ||
         (self.bits_.length > 0) && ((self.bits_[0] & 1) != 0);
};

var I_equals = function(self, other) {
  if (self.sign_ != other.sign_) {
    return false;
  }
  var len = Math.max(self.bits_.length, other.bits_.length);
  for (var i = 0; i < len; i++) {
    if (I_getBits(self, i) != I_getBits(other, i)) {
      return false;
    }
  }
  return true;
};

var I_notEquals = function(self, other) {
  return !I_equals(self, other);
};

var I_greaterThan = function(self, other) {
  return I_compare(self, other) > 0;
};

var I_greaterThanOrEqual = function(self, other) {
  return I_compare(self, other) >= 0;
};

var I_lessThan = function(self, other) {
  return I_compare(self, other) < 0;
};

var I_lessThanOrEqual = function(self, other) {
  return I_compare(self, other) <= 0;
};

var I_compare = function(self, other) {
  var diff = I_sub(self, other);
  if (isNegative(diff)) {
    return -1;
  } else if (isZero(diff)) {
    return 0;
  } else {
    return +1;
  }
};

var I_compareInt = function(self, other) {
  return I_compare(self, I_fromInt(other));
}

var shorten = function(self, numBits) {
  var arr_index = (numBits - 1) >> 5;
  var bit_index = (numBits - 1) % 32;
  var bits = [];
  for (var i = 0; i < arr_index; i++) {
    bits[i] = I_getBits(self, i);
  }
  var sigBits = bit_index == 31 ? 0xFFFFFFFF : (1 << (bit_index + 1)) - 1;
  var val = I_getBits(self, arr_index) & sigBits;
  if (val & (1 << bit_index)) {
    val |= 0xFFFFFFFF - sigBits;
    bits[arr_index] = val;
    return new Integer(bits, -1);
  } else {
    bits[arr_index] = val;
    return new Integer(bits, 0);
  }
};

var I_negate = function(self) {
  return I_add(not(self), Integer.ONE);
};

var I_add = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  var carry = 0;

  for (var i = 0; i <= len; i++) {
    var a1 = I_getBits(self, i) >>> 16;
    var a0 = I_getBits(self, i) & 0xFFFF;

    var b1 = I_getBits(other, i) >>> 16;
    var b0 = I_getBits(other, i) & 0xFFFF;

    var c0 = carry + a0 + b0;
    var c1 = (c0 >>> 16) + a1 + b1;
    carry = c1 >>> 16;
    c0 &= 0xFFFF;
    c1 &= 0xFFFF;
    arr[i] = (c1 << 16) | c0;
  }
  return I_fromBits(arr);
};

var I_sub = function(self, other) {
  return I_add(self, I_negate(other));
};

var I_mul = function(self, other) {
  if (isZero(self)) {
    return Integer.ZERO;
  } else if (isZero(other)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_mul(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_mul(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_mul(self, I_negate(other)));
  }

  if (I_lessThan(self, Integer.TWO_PWR_24_) &&
      I_lessThan(other, Integer.TWO_PWR_24_)) {
    return I_fromNumber(I_toNumber(self) * I_toNumber(other));
  }

  var len = self.bits_.length + other.bits_.length;
  var arr = [];
  for (var i = 0; i < 2 * len; i++) {
    arr[i] = 0;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    for (var j = 0; j < other.bits_.length; j++) {
      var a1 = I_getBits(self, i) >>> 16;
      var a0 = I_getBits(self, i) & 0xFFFF;

      var b1 = I_getBits(other, j) >>> 16;
      var b0 = I_getBits(other, j) & 0xFFFF;

      arr[2 * i + 2 * j] += a0 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j);
      arr[2 * i + 2 * j + 1] += a1 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 1] += a0 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 2] += a1 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 2);
    }
  }

  for (var i = 0; i < len; i++) {
    arr[i] = (arr[2 * i + 1] << 16) | arr[2 * i];
  }
  for (var i = len; i < 2 * len; i++) {
    arr[i] = 0;
  }
  return new Integer(arr, 0);
};

Integer.carry16_ = function(bits, index) {
  while ((bits[index] & 0xFFFF) != bits[index]) {
    bits[index + 1] += bits[index] >>> 16;
    bits[index] &= 0xFFFF;
  }
};

var I_mod = function(self, other) {
  return I_rem(I_add(other, I_rem(self, other)), other);
}

var I_div = function(self, other) {
  if(I_greaterThan(self, Integer.ZERO) != I_greaterThan(other, Integer.ZERO)) {
    if(I_rem(self, other) != Integer.ZERO) {
      return I_sub(I_quot(self, other), Integer.ONE);
    }
  }
  return I_quot(self, other);
}

var I_quotRem = function(self, other) {
  return [0, I_quot(self, other), I_rem(self, other)];
}

var I_divMod = function(self, other) {
  return [0, I_div(self, other), I_mod(self, other)];
}

var I_quot = function(self, other) {
  if (isZero(other)) {
    throw Error('division by zero');
  } else if (isZero(self)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_quot(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_quot(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_quot(self, I_negate(other)));
  }

  var res = Integer.ZERO;
  var rem = self;
  while (I_greaterThanOrEqual(rem, other)) {
    var approx = Math.max(1, Math.floor(I_toNumber(rem) / I_toNumber(other)));
    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);
    var approxRes = I_fromNumber(approx);
    var approxRem = I_mul(approxRes, other);
    while (isNegative(approxRem) || I_greaterThan(approxRem, rem)) {
      approx -= delta;
      approxRes = I_fromNumber(approx);
      approxRem = I_mul(approxRes, other);
    }

    if (isZero(approxRes)) {
      approxRes = Integer.ONE;
    }

    res = I_add(res, approxRes);
    rem = I_sub(rem, approxRem);
  }
  return res;
};

var I_rem = function(self, other) {
  return I_sub(self, I_mul(I_quot(self, other), other));
};

var not = function(self) {
  var len = self.bits_.length;
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = ~self.bits_[i];
  }
  return new Integer(arr, ~self.sign_);
};

var I_and = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) & I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ & other.sign_);
};

var I_or = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) | I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ | other.sign_);
};

var I_xor = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) ^ I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ ^ other.sign_);
};

var I_shiftLeft = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length + arr_delta + (bit_delta > 0 ? 1 : 0);
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i - arr_delta) << bit_delta) |
               (I_getBits(self, i - arr_delta - 1) >>> (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i - arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_shiftRight = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length - arr_delta;
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i + arr_delta) >>> bit_delta) |
               (I_getBits(self, i + arr_delta + 1) << (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i + arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_signum = function(self) {
  var cmp = I_compare(self, Integer.ZERO);
  if(cmp > 0) {
    return Integer.ONE
  }
  if(cmp < 0) {
    return I_sub(Integer.ZERO, Integer.ONE);
  }
  return Integer.ZERO;
};

var I_abs = function(self) {
  if(I_compare(self, Integer.ZERO) < 0) {
    return I_sub(Integer.ZERO, self);
  }
  return self;
};

var I_decodeDouble = function(x) {
  var dec = decodeDouble(x);
  var mantissa = I_fromBits([dec[3], dec[2]]);
  if(dec[1] < 0) {
    mantissa = I_negate(mantissa);
  }
  return [0, dec[4], mantissa];
}

var I_toString = function(self) {
  var radix = 10;

  if (isZero(self)) {
    return '0';
  } else if (isNegative(self)) {
    return '-' + I_toString(I_negate(self));
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 6));

  var rem = self;
  var result = '';
  while (true) {
    var remDiv = I_div(rem, radixToPower);
    var intval = I_toInt(I_sub(rem, I_mul(remDiv, radixToPower)));
    var digits = intval.toString();

    rem = remDiv;
    if (isZero(rem)) {
      return digits + result;
    } else {
      while (digits.length < 6) {
        digits = '0' + digits;
      }
      result = '' + digits + result;
    }
  }
};

var I_fromRat = function(a, b) {
    return I_toNumber(a) / I_toNumber(b);
}

function I_fromInt64(x) {
    return I_fromBits([x.getLowBits(), x.getHighBits()]);
}

function I_toInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

function I_fromWord64(x) {
    return x;
}

function I_toWord64(x) {
    return I_rem(I_add(__w64_max, x), __w64_max);
}

// Copyright 2009 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

var Long = function(low, high) {
  this.low_ = low | 0;
  this.high_ = high | 0;
};

Long.IntCache_ = {};

Long.fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Long.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Long(value | 0, value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Long.IntCache_[value] = obj;
  }
  return obj;
};

Long.fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Long.ZERO;
  } else if (value <= -Long.TWO_PWR_63_DBL_) {
    return Long.MIN_VALUE;
  } else if (value + 1 >= Long.TWO_PWR_63_DBL_) {
    return Long.MAX_VALUE;
  } else if (value < 0) {
    return Long.fromNumber(-value).negate();
  } else {
    return new Long(
        (value % Long.TWO_PWR_32_DBL_) | 0,
        (value / Long.TWO_PWR_32_DBL_) | 0);
  }
};

Long.fromBits = function(lowBits, highBits) {
  return new Long(lowBits, highBits);
};

Long.TWO_PWR_16_DBL_ = 1 << 16;
Long.TWO_PWR_24_DBL_ = 1 << 24;
Long.TWO_PWR_32_DBL_ =
    Long.TWO_PWR_16_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_31_DBL_ =
    Long.TWO_PWR_32_DBL_ / 2;
Long.TWO_PWR_48_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_64_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_32_DBL_;
Long.TWO_PWR_63_DBL_ =
    Long.TWO_PWR_64_DBL_ / 2;
Long.ZERO = Long.fromInt(0);
Long.ONE = Long.fromInt(1);
Long.NEG_ONE = Long.fromInt(-1);
Long.MAX_VALUE =
    Long.fromBits(0xFFFFFFFF | 0, 0x7FFFFFFF | 0);
Long.MIN_VALUE = Long.fromBits(0, 0x80000000 | 0);
Long.TWO_PWR_24_ = Long.fromInt(1 << 24);

Long.prototype.toInt = function() {
  return this.low_;
};

Long.prototype.toNumber = function() {
  return this.high_ * Long.TWO_PWR_32_DBL_ +
         this.getLowBitsUnsigned();
};

Long.prototype.getHighBits = function() {
  return this.high_;
};

Long.prototype.getLowBits = function() {
  return this.low_;
};

Long.prototype.getLowBitsUnsigned = function() {
  return (this.low_ >= 0) ?
      this.low_ : Long.TWO_PWR_32_DBL_ + this.low_;
};

Long.prototype.isZero = function() {
  return this.high_ == 0 && this.low_ == 0;
};

Long.prototype.isNegative = function() {
  return this.high_ < 0;
};

Long.prototype.isOdd = function() {
  return (this.low_ & 1) == 1;
};

Long.prototype.equals = function(other) {
  return (this.high_ == other.high_) && (this.low_ == other.low_);
};

Long.prototype.notEquals = function(other) {
  return (this.high_ != other.high_) || (this.low_ != other.low_);
};

Long.prototype.lessThan = function(other) {
  return this.compare(other) < 0;
};

Long.prototype.lessThanOrEqual = function(other) {
  return this.compare(other) <= 0;
};

Long.prototype.greaterThan = function(other) {
  return this.compare(other) > 0;
};

Long.prototype.greaterThanOrEqual = function(other) {
  return this.compare(other) >= 0;
};

Long.prototype.compare = function(other) {
  if (this.equals(other)) {
    return 0;
  }

  var thisNeg = this.isNegative();
  var otherNeg = other.isNegative();
  if (thisNeg && !otherNeg) {
    return -1;
  }
  if (!thisNeg && otherNeg) {
    return 1;
  }

  if (this.subtract(other).isNegative()) {
    return -1;
  } else {
    return 1;
  }
};

Long.prototype.negate = function() {
  if (this.equals(Long.MIN_VALUE)) {
    return Long.MIN_VALUE;
  } else {
    return this.not().add(Long.ONE);
  }
};

Long.prototype.add = function(other) {
  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 + b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 + b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 + b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 + b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.subtract = function(other) {
  return this.add(other.negate());
};

Long.prototype.multiply = function(other) {
  if (this.isZero()) {
    return Long.ZERO;
  } else if (other.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    return other.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  } else if (other.equals(Long.MIN_VALUE)) {
    return this.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().multiply(other.negate());
    } else {
      return this.negate().multiply(other).negate();
    }
  } else if (other.isNegative()) {
    return this.multiply(other.negate()).negate();
  }

  if (this.lessThan(Long.TWO_PWR_24_) &&
      other.lessThan(Long.TWO_PWR_24_)) {
    return Long.fromNumber(this.toNumber() * other.toNumber());
  }

  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 * b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 * b00;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c16 += a00 * b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 * b00;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a16 * b16;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a00 * b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.div = function(other) {
  if (other.isZero()) {
    throw Error('division by zero');
  } else if (this.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    if (other.equals(Long.ONE) ||
        other.equals(Long.NEG_ONE)) {
      return Long.MIN_VALUE;
    } else if (other.equals(Long.MIN_VALUE)) {
      return Long.ONE;
    } else {
      var halfThis = this.shiftRight(1);
      var approx = halfThis.div(other).shiftLeft(1);
      if (approx.equals(Long.ZERO)) {
        return other.isNegative() ? Long.ONE : Long.NEG_ONE;
      } else {
        var rem = this.subtract(other.multiply(approx));
        var result = approx.add(rem.div(other));
        return result;
      }
    }
  } else if (other.equals(Long.MIN_VALUE)) {
    return Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().div(other.negate());
    } else {
      return this.negate().div(other).negate();
    }
  } else if (other.isNegative()) {
    return this.div(other.negate()).negate();
  }

  var res = Long.ZERO;
  var rem = this;
  while (rem.greaterThanOrEqual(other)) {
    var approx = Math.max(1, Math.floor(rem.toNumber() / other.toNumber()));

    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);

    var approxRes = Long.fromNumber(approx);
    var approxRem = approxRes.multiply(other);
    while (approxRem.isNegative() || approxRem.greaterThan(rem)) {
      approx -= delta;
      approxRes = Long.fromNumber(approx);
      approxRem = approxRes.multiply(other);
    }

    if (approxRes.isZero()) {
      approxRes = Long.ONE;
    }

    res = res.add(approxRes);
    rem = rem.subtract(approxRem);
  }
  return res;
};

Long.prototype.modulo = function(other) {
  return this.subtract(this.div(other).multiply(other));
};

Long.prototype.not = function() {
  return Long.fromBits(~this.low_, ~this.high_);
};

Long.prototype.and = function(other) {
  return Long.fromBits(this.low_ & other.low_,
                                 this.high_ & other.high_);
};

Long.prototype.or = function(other) {
  return Long.fromBits(this.low_ | other.low_,
                                 this.high_ | other.high_);
};

Long.prototype.xor = function(other) {
  return Long.fromBits(this.low_ ^ other.low_,
                                 this.high_ ^ other.high_);
};

Long.prototype.shiftLeft = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var low = this.low_;
    if (numBits < 32) {
      var high = this.high_;
      return Long.fromBits(
          low << numBits,
          (high << numBits) | (low >>> (32 - numBits)));
    } else {
      return Long.fromBits(0, low << (numBits - 32));
    }
  }
};

Long.prototype.shiftRight = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >> numBits);
    } else {
      return Long.fromBits(
          high >> (numBits - 32),
          high >= 0 ? 0 : -1);
    }
  }
};

Long.prototype.shiftRightUnsigned = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >>> numBits);
    } else if (numBits == 32) {
      return Long.fromBits(high, 0);
    } else {
      return Long.fromBits(high >>> (numBits - 32), 0);
    }
  }
};



// Int64
function hs_eqInt64(x, y) {return x.equals(y);}
function hs_neInt64(x, y) {return !x.equals(y);}
function hs_ltInt64(x, y) {return x.compare(y) < 0;}
function hs_leInt64(x, y) {return x.compare(y) <= 0;}
function hs_gtInt64(x, y) {return x.compare(y) > 0;}
function hs_geInt64(x, y) {return x.compare(y) >= 0;}
function hs_quotInt64(x, y) {return x.div(y);}
function hs_remInt64(x, y) {return x.modulo(y);}
function hs_plusInt64(x, y) {return x.add(y);}
function hs_minusInt64(x, y) {return x.subtract(y);}
function hs_timesInt64(x, y) {return x.multiply(y);}
function hs_negateInt64(x) {return x.negate();}
function hs_uncheckedIShiftL64(x, bits) {return x.shiftLeft(bits);}
function hs_uncheckedIShiftRA64(x, bits) {return x.shiftRight(bits);}
function hs_uncheckedIShiftRL64(x, bits) {return x.shiftRightUnsigned(bits);}
function hs_intToInt64(x) {return new Long(x, 0);}
function hs_int64ToInt(x) {return x.toInt();}



// Word64
function hs_wordToWord64(x) {
    return I_fromInt(x);
}
function hs_word64ToWord(x) {
    return I_toInt(x);
}
function hs_mkWord64(low, high) {
    return I_fromBits([low, high]);
}

var hs_and64 = I_and;
var hs_or64 = I_or;
var hs_xor64 = I_xor;
var __i64_all_ones = I_fromBits([0xffffffff, 0xffffffff]);
function hs_not64(x) {
    return I_xor(x, __i64_all_ones);
}
var hs_eqWord64 = I_equals;
var hs_neWord64 = I_notEquals;
var hs_ltWord64 = I_lessThan;
var hs_leWord64 = I_lessThanOrEqual;
var hs_gtWord64 = I_greaterThan;
var hs_geWord64 = I_greaterThanOrEqual;
var hs_quotWord64 = I_quot;
var hs_remWord64 = I_rem;
var __w64_max = I_fromBits([0,0,1]);
function hs_uncheckedShiftL64(x, bits) {
    return I_rem(I_shiftLeft(x, bits), __w64_max);
}
var hs_uncheckedShiftRL64 = I_shiftRight;
function hs_int64ToWord64(x) {
    var tmp = I_add(__w64_max, I_fromBits([x.getLowBits(), x.getHighBits()]));
    return I_rem(tmp, __w64_max);
}
function hs_word64ToInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

// Joseph Myers' MD5 implementation; used under the BSD license.

function md5cycle(x, k) {
    var a = x[0], b = x[1], c = x[2], d = x[3];

    a = ff(a, b, c, d, k[0], 7, -680876936);
    d = ff(d, a, b, c, k[1], 12, -389564586);
    c = ff(c, d, a, b, k[2], 17,  606105819);
    b = ff(b, c, d, a, k[3], 22, -1044525330);
    a = ff(a, b, c, d, k[4], 7, -176418897);
    d = ff(d, a, b, c, k[5], 12,  1200080426);
    c = ff(c, d, a, b, k[6], 17, -1473231341);
    b = ff(b, c, d, a, k[7], 22, -45705983);
    a = ff(a, b, c, d, k[8], 7,  1770035416);
    d = ff(d, a, b, c, k[9], 12, -1958414417);
    c = ff(c, d, a, b, k[10], 17, -42063);
    b = ff(b, c, d, a, k[11], 22, -1990404162);
    a = ff(a, b, c, d, k[12], 7,  1804603682);
    d = ff(d, a, b, c, k[13], 12, -40341101);
    c = ff(c, d, a, b, k[14], 17, -1502002290);
    b = ff(b, c, d, a, k[15], 22,  1236535329);

    a = gg(a, b, c, d, k[1], 5, -165796510);
    d = gg(d, a, b, c, k[6], 9, -1069501632);
    c = gg(c, d, a, b, k[11], 14,  643717713);
    b = gg(b, c, d, a, k[0], 20, -373897302);
    a = gg(a, b, c, d, k[5], 5, -701558691);
    d = gg(d, a, b, c, k[10], 9,  38016083);
    c = gg(c, d, a, b, k[15], 14, -660478335);
    b = gg(b, c, d, a, k[4], 20, -405537848);
    a = gg(a, b, c, d, k[9], 5,  568446438);
    d = gg(d, a, b, c, k[14], 9, -1019803690);
    c = gg(c, d, a, b, k[3], 14, -187363961);
    b = gg(b, c, d, a, k[8], 20,  1163531501);
    a = gg(a, b, c, d, k[13], 5, -1444681467);
    d = gg(d, a, b, c, k[2], 9, -51403784);
    c = gg(c, d, a, b, k[7], 14,  1735328473);
    b = gg(b, c, d, a, k[12], 20, -1926607734);

    a = hh(a, b, c, d, k[5], 4, -378558);
    d = hh(d, a, b, c, k[8], 11, -2022574463);
    c = hh(c, d, a, b, k[11], 16,  1839030562);
    b = hh(b, c, d, a, k[14], 23, -35309556);
    a = hh(a, b, c, d, k[1], 4, -1530992060);
    d = hh(d, a, b, c, k[4], 11,  1272893353);
    c = hh(c, d, a, b, k[7], 16, -155497632);
    b = hh(b, c, d, a, k[10], 23, -1094730640);
    a = hh(a, b, c, d, k[13], 4,  681279174);
    d = hh(d, a, b, c, k[0], 11, -358537222);
    c = hh(c, d, a, b, k[3], 16, -722521979);
    b = hh(b, c, d, a, k[6], 23,  76029189);
    a = hh(a, b, c, d, k[9], 4, -640364487);
    d = hh(d, a, b, c, k[12], 11, -421815835);
    c = hh(c, d, a, b, k[15], 16,  530742520);
    b = hh(b, c, d, a, k[2], 23, -995338651);

    a = ii(a, b, c, d, k[0], 6, -198630844);
    d = ii(d, a, b, c, k[7], 10,  1126891415);
    c = ii(c, d, a, b, k[14], 15, -1416354905);
    b = ii(b, c, d, a, k[5], 21, -57434055);
    a = ii(a, b, c, d, k[12], 6,  1700485571);
    d = ii(d, a, b, c, k[3], 10, -1894986606);
    c = ii(c, d, a, b, k[10], 15, -1051523);
    b = ii(b, c, d, a, k[1], 21, -2054922799);
    a = ii(a, b, c, d, k[8], 6,  1873313359);
    d = ii(d, a, b, c, k[15], 10, -30611744);
    c = ii(c, d, a, b, k[6], 15, -1560198380);
    b = ii(b, c, d, a, k[13], 21,  1309151649);
    a = ii(a, b, c, d, k[4], 6, -145523070);
    d = ii(d, a, b, c, k[11], 10, -1120210379);
    c = ii(c, d, a, b, k[2], 15,  718787259);
    b = ii(b, c, d, a, k[9], 21, -343485551);

    x[0] = add32(a, x[0]);
    x[1] = add32(b, x[1]);
    x[2] = add32(c, x[2]);
    x[3] = add32(d, x[3]);

}

function cmn(q, a, b, x, s, t) {
    a = add32(add32(a, q), add32(x, t));
    return add32((a << s) | (a >>> (32 - s)), b);
}

function ff(a, b, c, d, x, s, t) {
    return cmn((b & c) | ((~b) & d), a, b, x, s, t);
}

function gg(a, b, c, d, x, s, t) {
    return cmn((b & d) | (c & (~d)), a, b, x, s, t);
}

function hh(a, b, c, d, x, s, t) {
    return cmn(b ^ c ^ d, a, b, x, s, t);
}

function ii(a, b, c, d, x, s, t) {
    return cmn(c ^ (b | (~d)), a, b, x, s, t);
}

function md51(s) {
    var n = s.length,
        state = [1732584193, -271733879, -1732584194, 271733878], i;
    for (i=64; i<=s.length; i+=64) {
        md5cycle(state, md5blk(s.substring(i-64, i)));
    }
    s = s.substring(i-64);
    var tail = [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0];
    for (i=0; i<s.length; i++)
        tail[i>>2] |= s.charCodeAt(i) << ((i%4) << 3);
    tail[i>>2] |= 0x80 << ((i%4) << 3);
    if (i > 55) {
        md5cycle(state, tail);
        for (i=0; i<16; i++) tail[i] = 0;
    }
    tail[14] = n*8;
    md5cycle(state, tail);
    return state;
}
window['md51'] = md51;

function md5blk(s) {
    var md5blks = [], i;
    for (i=0; i<64; i+=4) {
        md5blks[i>>2] = s.charCodeAt(i)
            + (s.charCodeAt(i+1) << 8)
            + (s.charCodeAt(i+2) << 16)
            + (s.charCodeAt(i+3) << 24);
    }
    return md5blks;
}

var hex_chr = '0123456789abcdef'.split('');

function rhex(n)
{
    var s='', j=0;
    for(; j<4; j++)
        s += hex_chr[(n >> (j * 8 + 4)) & 0x0F]
        + hex_chr[(n >> (j * 8)) & 0x0F];
    return s;
}

function hex(x) {
    for (var i=0; i<x.length; i++)
        x[i] = rhex(x[i]);
    return x.join('');
}

function md5(s) {
    return hex(md51(s));
}

function add32(a, b) {
    return (a + b) & 0xFFFFFFFF;
}

// Functions for dealing with arrays.

function newArr(n, x) {
    var arr = [];
    for(; n >= 0; --n) {
        arr.push(x);
    }
    return arr;
}

// Create all views at once; perhaps it's wasteful, but it's better than having
// to check for the right view at each read or write.
function newByteArr(n) {
    // Pad the thing to multiples of 8.
    var padding = 8 - n % 8;
    if(padding < 8) {
        n += padding;
    }
    var arr = {};
    var buffer = new ArrayBuffer(n);
    var views = {};
    views['i8']  = new Int8Array(buffer);
    views['i16'] = new Int16Array(buffer);
    views['i32'] = new Int32Array(buffer);
    views['w8']  = new Uint8Array(buffer);
    views['w16'] = new Uint16Array(buffer);
    views['w32'] = new Uint32Array(buffer);
    views['f32'] = new Float32Array(buffer);
    views['f64'] = new Float64Array(buffer);
    arr['b'] = buffer;
    arr['v'] = views;
    // ByteArray and Addr are the same thing, so keep an offset if we get
    // casted.
    arr['off'] = 0;
    return arr;
}

// An attempt at emulating pointers enough for ByteString and Text to be
// usable without patching the hell out of them.
// The general idea is that Addr# is a byte array with an associated offset.

function plusAddr(addr, off) {
    var newaddr = {};
    newaddr['off'] = addr['off'] + off;
    newaddr['b']   = addr['b'];
    newaddr['v']   = addr['v'];
    return newaddr;
}

function writeOffAddr(type, elemsize, addr, off, x) {
    addr['v'][type][addr.off/elemsize + off] = x;
}

function readOffAddr(type, elemsize, addr, off) {
    return addr['v'][type][addr.off/elemsize + off];
}

// Two addresses are equal if they point to the same buffer and have the same
// offset. For other comparisons, just use the offsets - nobody in their right
// mind would check if one pointer is less than another, completely unrelated,
// pointer and then act on that information anyway.
function addrEq(a, b) {
    if(a == b) {
        return true;
    }
    return a && b && a['b'] == b['b'] && a['off'] == b['off'];
}

function addrLT(a, b) {
    if(a) {
        return b && a['off'] < b['off'];
    } else {
        return (b != 0); 
    }
}

function addrGT(a, b) {
    if(b) {
        return a && a['off'] > b['off'];
    } else {
        return (a != 0);
    }
}

function withChar(f, charCode) {
    return f(String.fromCharCode(charCode)).charCodeAt(0);
}

function u_towlower(charCode) {
    return withChar(function(c) {return c.toLowerCase()}, charCode);
}

function u_towupper(charCode) {
    return withChar(function(c) {return c.toUpperCase()}, charCode);
}

var u_towtitle = u_towupper;

function u_iswupper(charCode) {
    var c = String.fromCharCode(charCode);
    return c == c.toUpperCase() && c != c.toLowerCase();
}

function u_iswlower(charCode) {
    var c = String.fromCharCode(charCode);
    return  c == c.toLowerCase() && c != c.toUpperCase();
}

function u_iswdigit(charCode) {
    return charCode >= 48 && charCode <= 57;
}

function u_iswcntrl(charCode) {
    return charCode <= 0x1f || charCode == 0x7f;
}

function u_iswspace(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(/\s/g,'') != c;
}

function u_iswalpha(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(__hs_alphare, '') != c;
}

function u_iswalnum(charCode) {
    return u_iswdigit(charCode) || u_iswalpha(charCode);
}

function u_iswprint(charCode) {
    return !u_iswcntrl(charCode);
}

function u_gencat(c) {
    throw 'u_gencat is only supported with --full-unicode.';
}

// Regex that matches any alphabetic character in any language. Horrible thing.
var __hs_alphare = /[\u0041-\u005A\u0061-\u007A\u00AA\u00B5\u00BA\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02C1\u02C6-\u02D1\u02E0-\u02E4\u02EC\u02EE\u0370-\u0374\u0376\u0377\u037A-\u037D\u0386\u0388-\u038A\u038C\u038E-\u03A1\u03A3-\u03F5\u03F7-\u0481\u048A-\u0527\u0531-\u0556\u0559\u0561-\u0587\u05D0-\u05EA\u05F0-\u05F2\u0620-\u064A\u066E\u066F\u0671-\u06D3\u06D5\u06E5\u06E6\u06EE\u06EF\u06FA-\u06FC\u06FF\u0710\u0712-\u072F\u074D-\u07A5\u07B1\u07CA-\u07EA\u07F4\u07F5\u07FA\u0800-\u0815\u081A\u0824\u0828\u0840-\u0858\u08A0\u08A2-\u08AC\u0904-\u0939\u093D\u0950\u0958-\u0961\u0971-\u0977\u0979-\u097F\u0985-\u098C\u098F\u0990\u0993-\u09A8\u09AA-\u09B0\u09B2\u09B6-\u09B9\u09BD\u09CE\u09DC\u09DD\u09DF-\u09E1\u09F0\u09F1\u0A05-\u0A0A\u0A0F\u0A10\u0A13-\u0A28\u0A2A-\u0A30\u0A32\u0A33\u0A35\u0A36\u0A38\u0A39\u0A59-\u0A5C\u0A5E\u0A72-\u0A74\u0A85-\u0A8D\u0A8F-\u0A91\u0A93-\u0AA8\u0AAA-\u0AB0\u0AB2\u0AB3\u0AB5-\u0AB9\u0ABD\u0AD0\u0AE0\u0AE1\u0B05-\u0B0C\u0B0F\u0B10\u0B13-\u0B28\u0B2A-\u0B30\u0B32\u0B33\u0B35-\u0B39\u0B3D\u0B5C\u0B5D\u0B5F-\u0B61\u0B71\u0B83\u0B85-\u0B8A\u0B8E-\u0B90\u0B92-\u0B95\u0B99\u0B9A\u0B9C\u0B9E\u0B9F\u0BA3\u0BA4\u0BA8-\u0BAA\u0BAE-\u0BB9\u0BD0\u0C05-\u0C0C\u0C0E-\u0C10\u0C12-\u0C28\u0C2A-\u0C33\u0C35-\u0C39\u0C3D\u0C58\u0C59\u0C60\u0C61\u0C85-\u0C8C\u0C8E-\u0C90\u0C92-\u0CA8\u0CAA-\u0CB3\u0CB5-\u0CB9\u0CBD\u0CDE\u0CE0\u0CE1\u0CF1\u0CF2\u0D05-\u0D0C\u0D0E-\u0D10\u0D12-\u0D3A\u0D3D\u0D4E\u0D60\u0D61\u0D7A-\u0D7F\u0D85-\u0D96\u0D9A-\u0DB1\u0DB3-\u0DBB\u0DBD\u0DC0-\u0DC6\u0E01-\u0E30\u0E32\u0E33\u0E40-\u0E46\u0E81\u0E82\u0E84\u0E87\u0E88\u0E8A\u0E8D\u0E94-\u0E97\u0E99-\u0E9F\u0EA1-\u0EA3\u0EA5\u0EA7\u0EAA\u0EAB\u0EAD-\u0EB0\u0EB2\u0EB3\u0EBD\u0EC0-\u0EC4\u0EC6\u0EDC-\u0EDF\u0F00\u0F40-\u0F47\u0F49-\u0F6C\u0F88-\u0F8C\u1000-\u102A\u103F\u1050-\u1055\u105A-\u105D\u1061\u1065\u1066\u106E-\u1070\u1075-\u1081\u108E\u10A0-\u10C5\u10C7\u10CD\u10D0-\u10FA\u10FC-\u1248\u124A-\u124D\u1250-\u1256\u1258\u125A-\u125D\u1260-\u1288\u128A-\u128D\u1290-\u12B0\u12B2-\u12B5\u12B8-\u12BE\u12C0\u12C2-\u12C5\u12C8-\u12D6\u12D8-\u1310\u1312-\u1315\u1318-\u135A\u1380-\u138F\u13A0-\u13F4\u1401-\u166C\u166F-\u167F\u1681-\u169A\u16A0-\u16EA\u1700-\u170C\u170E-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176C\u176E-\u1770\u1780-\u17B3\u17D7\u17DC\u1820-\u1877\u1880-\u18A8\u18AA\u18B0-\u18F5\u1900-\u191C\u1950-\u196D\u1970-\u1974\u1980-\u19AB\u19C1-\u19C7\u1A00-\u1A16\u1A20-\u1A54\u1AA7\u1B05-\u1B33\u1B45-\u1B4B\u1B83-\u1BA0\u1BAE\u1BAF\u1BBA-\u1BE5\u1C00-\u1C23\u1C4D-\u1C4F\u1C5A-\u1C7D\u1CE9-\u1CEC\u1CEE-\u1CF1\u1CF5\u1CF6\u1D00-\u1DBF\u1E00-\u1F15\u1F18-\u1F1D\u1F20-\u1F45\u1F48-\u1F4D\u1F50-\u1F57\u1F59\u1F5B\u1F5D\u1F5F-\u1F7D\u1F80-\u1FB4\u1FB6-\u1FBC\u1FBE\u1FC2-\u1FC4\u1FC6-\u1FCC\u1FD0-\u1FD3\u1FD6-\u1FDB\u1FE0-\u1FEC\u1FF2-\u1FF4\u1FF6-\u1FFC\u2071\u207F\u2090-\u209C\u2102\u2107\u210A-\u2113\u2115\u2119-\u211D\u2124\u2126\u2128\u212A-\u212D\u212F-\u2139\u213C-\u213F\u2145-\u2149\u214E\u2183\u2184\u2C00-\u2C2E\u2C30-\u2C5E\u2C60-\u2CE4\u2CEB-\u2CEE\u2CF2\u2CF3\u2D00-\u2D25\u2D27\u2D2D\u2D30-\u2D67\u2D6F\u2D80-\u2D96\u2DA0-\u2DA6\u2DA8-\u2DAE\u2DB0-\u2DB6\u2DB8-\u2DBE\u2DC0-\u2DC6\u2DC8-\u2DCE\u2DD0-\u2DD6\u2DD8-\u2DDE\u2E2F\u3005\u3006\u3031-\u3035\u303B\u303C\u3041-\u3096\u309D-\u309F\u30A1-\u30FA\u30FC-\u30FF\u3105-\u312D\u3131-\u318E\u31A0-\u31BA\u31F0-\u31FF\u3400-\u4DB5\u4E00-\u9FCC\uA000-\uA48C\uA4D0-\uA4FD\uA500-\uA60C\uA610-\uA61F\uA62A\uA62B\uA640-\uA66E\uA67F-\uA697\uA6A0-\uA6E5\uA717-\uA71F\uA722-\uA788\uA78B-\uA78E\uA790-\uA793\uA7A0-\uA7AA\uA7F8-\uA801\uA803-\uA805\uA807-\uA80A\uA80C-\uA822\uA840-\uA873\uA882-\uA8B3\uA8F2-\uA8F7\uA8FB\uA90A-\uA925\uA930-\uA946\uA960-\uA97C\uA984-\uA9B2\uA9CF\uAA00-\uAA28\uAA40-\uAA42\uAA44-\uAA4B\uAA60-\uAA76\uAA7A\uAA80-\uAAAF\uAAB1\uAAB5\uAAB6\uAAB9-\uAABD\uAAC0\uAAC2\uAADB-\uAADD\uAAE0-\uAAEA\uAAF2-\uAAF4\uAB01-\uAB06\uAB09-\uAB0E\uAB11-\uAB16\uAB20-\uAB26\uAB28-\uAB2E\uABC0-\uABE2\uAC00-\uD7A3\uD7B0-\uD7C6\uD7CB-\uD7FB\uF900-\uFA6D\uFA70-\uFAD9\uFB00-\uFB06\uFB13-\uFB17\uFB1D\uFB1F-\uFB28\uFB2A-\uFB36\uFB38-\uFB3C\uFB3E\uFB40\uFB41\uFB43\uFB44\uFB46-\uFBB1\uFBD3-\uFD3D\uFD50-\uFD8F\uFD92-\uFDC7\uFDF0-\uFDFB\uFE70-\uFE74\uFE76-\uFEFC\uFF21-\uFF3A\uFF41-\uFF5A\uFF66-\uFFBE\uFFC2-\uFFC7\uFFCA-\uFFCF\uFFD2-\uFFD7\uFFDA-\uFFDC]/g;

// 2D Canvas drawing primitives.
function jsHasCtx2D(elem) {return !!elem.getContext;}
function jsGetCtx2D(elem) {return elem.getContext('2d');}
function jsBeginPath(ctx) {ctx.beginPath();}
function jsMoveTo(ctx, x, y) {ctx.moveTo(x, y);}
function jsLineTo(ctx, x, y) {ctx.lineTo(x, y);}
function jsStroke(ctx) {ctx.stroke();}
function jsFill(ctx) {ctx.fill();}
function jsRotate(ctx, radians) {ctx.rotate(radians);}
function jsTranslate(ctx, x, y) {ctx.translate(x, y);}
function jsScale(ctx, x, y) {ctx.scale(x, y);}
function jsPushState(ctx) {ctx.save();}
function jsPopState(ctx) {ctx.restore();}
function jsResetCanvas(el) {el.width = el.width;}
function jsDrawImage(ctx, img, x, y) {ctx.drawImage(img, x, y);}
function jsDrawImageClipped(ctx, img, x, y, cx, cy, cw, ch) {
    ctx.drawImage(img, cx, cy, cw, ch, x, y, cw, ch);
}
function jsDrawText(ctx, str, x, y) {ctx.fillText(str, x, y);}
function jsClip(ctx) {ctx.clip();}
function jsArc(ctx, x, y, radius, fromAngle, toAngle) {
    ctx.arc(x, y, radius, fromAngle, toAngle);
}
function jsCanvasToDataURL(el) {return el.toDataURL('image/png');}

// Simulate handles.
// When implementing new handles, remember that passed strings may be thunks,
// and so need to be evaluated before use.

function jsNewHandle(init, read, write, flush, close, seek, tell) {
    var h = {
        read: read || function() {},
        write: write || function() {},
        seek: seek || function() {},
        tell: tell || function() {},
        close: close || function() {},
        flush: flush || function() {}
    };
    init.call(h);
    return h;
}

function jsReadHandle(h, len) {return h.read(len);}
function jsWriteHandle(h, str) {return h.write(str);}
function jsFlushHandle(h) {return h.flush();}
function jsCloseHandle(h) {return h.close();}

function jsMkConWriter(op) {
    return function(str) {
        str = E(str);
        var lines = (this.buf + str).split('\n');
        for(var i = 0; i < lines.length-1; ++i) {
            op.call(console, lines[i]);
        }
        this.buf = lines[lines.length-1];
    }
}

function jsMkStdout() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.log),
        function() {console.log(this.buf); this.buf = '';}
    );
}

function jsMkStderr() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.warn),
        function() {console.warn(this.buf); this.buf = '';}
    );
}

function jsMkStdin() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(len) {
            while(this.buf.length < len) {
                this.buf += prompt('[stdin]') + '\n';
            }
            var ret = this.buf.substr(0, len);
            this.buf = this.buf.substr(len);
            return ret;
        }
    );
}

// "Weak Pointers". Mostly useless implementation since
// JS does its own GC.

function mkWeak(key, val, fin) {
    fin = !fin? function() {}: fin;
    return {key: key, val: val, fin: fin};
}

function derefWeak(w) {
    return [0, 1, E(w).val];
}

function finalizeWeak(w) {
    return [0, B(A(E(w).fin, [0]))];
}

var _0=function(_1,_2,_){var _3=jsCreateTextNode(toJSStr(E(_1))),_4=_3,_5=jsAppendChild(_4,E(_2)[1]);return [0,_4];},_6=function(_7){var _8=B(A(_7,[_])),_9=_8;return E(_9);},_a=function(_b){return new F(function(){return _6(function(_){var _=0;return new F(function(){return eval(_b);});});});},_c=0,_d=function(_e,_f,_g,_h){return new F(function(){return A(_e,[function(_){var _i=jsSetAttr(E(_f)[1],toJSStr(E(_g)),toJSStr(E(_h)));return _c;}]);});},_j=new T(function(){return B(unCStr("stylesheet"));}),_k=new T(function(){return B(unCStr("rel"));}),_l=new T(function(){return B(unCStr("(function(){return document.head;})"));}),_m=function(_n,_){var _o=B(A(_a,[toJSStr(E(_l)),_])),_p=_o,_q=B(A(_n,[[0,_p],_])),_r=_q;return _c;},_s=new T(function(){return B(unCStr("href"));}),_t=function(_u){return E(_u);},_v=new T(function(){return B(unCStr("link"));}),_w=function(_x,_){var _y=jsCreateElem(toJSStr(E(_v))),_z=_y,_A=jsAppendChild(_z,E(_x)[1]);return [0,_z];},_B=function(_C,_){return new F(function(){return _m(function(_D,_){var _E=B(_w(_D,_)),_F=_E,_G=B(A(_d,[_t,_F,_k,_j,_])),_H=_G,_I=B(A(_d,[_t,_F,_s,_C,_])),_J=_I;return _F;},_);});},_K=function(_L,_){return _L;},_M=new T(function(){return B(unCStr("script"));}),_N=function(_O,_P,_Q,_){var _R=jsCreateElem(toJSStr(E(_M))),_S=_R,_T=jsAppendChild(_S,E(_Q)[1]),_U=[0,_S],_V=B(A(_O,[_P,_U,_])),_W=_V;return _U;},_X=new T(function(){return B(unCStr("src"));}),_Y=function(_Z){return E(_Z);},_10=function(_11,_){return new F(function(){return _m(function(_12,_){var _13=B(_N(_Y,_K,_12,_)),_14=_13,_15=B(A(_d,[_t,_14,_X,_11,_])),_16=_15;return _14;},_);});},_17=new T(function(){return B(unCStr("style"));}),_18=new T(function(){return B(unCStr("https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap-theme.min.css"));}),_19=new T(function(){return B(unCStr("https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css"));}),_1a=2,_1b=new T(function(){return B(unCStr("span"));}),_1c=new T(function(){return [0,"arr2lst"];}),_1d=function(_1e,_1f){return new F(function(){return _6(function(_){var _=0;return new F(function(){return A(_a,[E(_1c)[1],E(_1e),E(_1f),_]);});});});},_1g=[0],_1h=new T(function(){return B(_a("(function(sel){return document.querySelectorAll(sel);})"));}),_1i=function(_1j,_1k,_1l,_){var _1m=B(A(_1h,[E(toJSStr(E(_1j))),_])),_1n=_1m,_1o=function(_1p,_){var _1q=E(_1p);if(!_1q[0]){return _1g;}else{var _1r=B(A(_1k,[[0,_1q[1]],_])),_1s=_1r,_1t=B(_1o(_1q[2],_)),_1u=_1t;return [1,_1s,_1u];}},_1v=B(_1o(B(_1d(_1n,0)),_)),_1w=_1v;return _1l;},_1x=new T(function(){return B(unCStr("Prelude.undefined"));}),_1y=new T(function(){return B(err(_1x));}),_1z=function(_1A,_1B,_1C,_1D,_){var _1E=B(A(_1C,[_1D,_])),_1F=_1E,_1G=E(_1F),_1H=E(_1G[1]),_1I=_1H[1];return [0,[0,function(_1J,_){switch(E(_1B)){case 0:var _1K=B(_1i(_1A,_1I,_1y,_)),_1L=_1K;return _1J;case 1:var _1M=B(_1i(_1A,function(_1N,_){var _1O=E(_1N),_1P=_1O[1],_1Q=jsGetChildren(_1P),_1R=_1Q,_1S=E(_1R);if(!_1S[0]){var _1T=B(A(_1I,[_1O,_])),_1U=_1T;return _1O;}else{var _1V=jsCreateElem(toJSStr(E(_1b))),_1W=_1V,_1X=jsAddChildBefore(_1W,_1P,E(_1S[1])[1]),_1Y=B(A(_1I,[[0,_1W],_])),_1Z=_1Y;return _1O;}},_1y,_)),_20=_1M;return _1J;default:var _21=B(_1i(_1A,function(_22,_){var _23=E(_22),_24=jsClearChildren(_23[1]),_25=B(A(_1I,[_23,_])),_26=_25;return _23;},_1y,_)),_27=_21;return _1J;}},_1H[2]],_1G[2]];},_28=new T(function(){return B(unCStr("function(x)\n{\n    return x*Math.sin(x);\n}"));}),_29=[0,3],_2a=[0,40],_2b=[0,3],_2c=new T(function(){return B(unCStr("ArithException"));}),_2d=new T(function(){return B(unCStr("GHC.Exception"));}),_2e=new T(function(){return B(unCStr("base"));}),_2f=new T(function(){var _2g=hs_wordToWord64(4194982440),_2h=_2g,_2i=hs_wordToWord64(3110813675),_2j=_2i;return [0,_2h,_2j,[0,_2h,_2j,_2e,_2d,_2c],_1g];}),_2k=function(_2l){return E(_2f);},_2m=function(_2n){return E(E(_2n)[1]);},_2o=function(_2p,_2q,_2r){var _2s=B(A(_2p,[_])),_2t=B(A(_2q,[_])),_2u=hs_eqWord64(_2s[1],_2t[1]),_2v=_2u;if(!E(_2v)){return [0];}else{var _2w=hs_eqWord64(_2s[2],_2t[2]),_2x=_2w;return E(_2x)==0?[0]:[1,_2r];}},_2y=function(_2z){var _2A=E(_2z);return new F(function(){return _2o(B(_2m(_2A[1])),_2k,_2A[2]);});},_2B=new T(function(){return B(unCStr("arithmetic underflow"));}),_2C=new T(function(){return B(unCStr("arithmetic overflow"));}),_2D=new T(function(){return B(unCStr("Ratio has zero denominator"));}),_2E=new T(function(){return B(unCStr("denormal"));}),_2F=new T(function(){return B(unCStr("divide by zero"));}),_2G=new T(function(){return B(unCStr("loss of precision"));}),_2H=function(_2I){switch(E(_2I)){case 0:return E(_2C);case 1:return E(_2B);case 2:return E(_2G);case 3:return E(_2F);case 4:return E(_2E);default:return E(_2D);}},_2J=function(_2K,_2L){var _2M=E(_2K);return _2M[0]==0?E(_2L):[1,_2M[1],new T(function(){return B(_2J(_2M[2],_2L));})];},_2N=function(_2O){return new F(function(){return _2J(_2B,_2O);});},_2P=function(_2O){return new F(function(){return _2J(_2C,_2O);});},_2Q=function(_2O){return new F(function(){return _2J(_2D,_2O);});},_2R=function(_2O){return new F(function(){return _2J(_2E,_2O);});},_2S=function(_2O){return new F(function(){return _2J(_2F,_2O);});},_2T=function(_2O){return new F(function(){return _2J(_2G,_2O);});},_2U=function(_2V){switch(E(_2V)){case 0:return E(_2P);case 1:return E(_2N);case 2:return E(_2T);case 3:return E(_2S);case 4:return E(_2R);default:return E(_2Q);}},_2W=[0,44],_2X=[0,93],_2Y=[0,91],_2Z=function(_30,_31,_32){var _33=E(_31);return _33[0]==0?B(unAppCStr("[]",_32)):[1,_2Y,new T(function(){return B(A(_30,[_33[1],new T(function(){var _34=function(_35){var _36=E(_35);return _36[0]==0?E([1,_2X,_32]):[1,_2W,new T(function(){return B(A(_30,[_36[1],new T(function(){return B(_34(_36[2]));})]));})];};return B(_34(_33[2]));})]));})];},_37=function(_38,_39){return new F(function(){return _2Z(_2U,_38,_39);});},_3a=function(_3b,_3c){switch(E(_3c)){case 0:return E(_2P);case 1:return E(_2N);case 2:return E(_2T);case 3:return E(_2S);case 4:return E(_2R);default:return E(_2Q);}},_3d=[0,_3a,_2H,_37],_3e=new T(function(){return [0,_2k,_3d,_3f,_2y];}),_3f=function(_2O){return [0,_3e,_2O];},_3g=3,_3h=new T(function(){return B(_3f(_3g));}),_3i=new T(function(){return die(_3h);}),_3j=function(_3k,_3l){var _3m=E(_3k);if(!_3m[0]){var _3n=_3m[1],_3o=E(_3l);return _3o[0]==0?_3n==_3o[1]:I_compareInt(_3o[1],_3n)==0?true:false;}else{var _3p=_3m[1],_3q=E(_3l);return _3q[0]==0?I_compareInt(_3p,_3q[1])==0?true:false:I_compare(_3p,_3q[1])==0?true:false;}},_3r=[0,0],_3s=[0,2],_3t=new T(function(){return B(_3j(_3s,_3r));}),_3u=[0,1],_3v=[0,2],_3w=[0,0],_3x=new T(function(){return B(_3j(_3v,_3w));}),_3y=function(_3z,_3A){while(1){var _3B=E(_3z);if(!_3B[0]){var _3C=_3B[1],_3D=E(_3A);if(!_3D[0]){var _3E=_3D[1],_3F=subC(_3C,_3E);if(!E(_3F[2])){return [0,_3F[1]];}else{_3z=[1,I_fromInt(_3C)];_3A=[1,I_fromInt(_3E)];continue;}}else{_3z=[1,I_fromInt(_3C)];_3A=_3D;continue;}}else{var _3G=E(_3A);if(!_3G[0]){_3z=_3B;_3A=[1,I_fromInt(_3G[1])];continue;}else{return [1,I_sub(_3B[1],_3G[1])];}}}},_3H=function(_3I,_3J){while(1){var _3K=E(_3I);if(!_3K[0]){var _3L=E(_3K[1]);if(_3L==(-2147483648)){_3I=[1,I_fromInt(-2147483648)];continue;}else{var _3M=E(_3J);if(!_3M[0]){return [0,quot(_3L,_3M[1])];}else{_3I=[1,I_fromInt(_3L)];_3J=_3M;continue;}}}else{var _3N=_3K[1],_3O=E(_3J);return _3O[0]==0?[0,I_toInt(I_quot(_3N,I_fromInt(_3O[1])))]:[1,I_quot(_3N,_3O[1])];}}},_3P=function(_3Q,_3R){while(1){var _3S=E(_3Q);if(!_3S[0]){var _3T=E(_3S[1]);if(_3T==(-2147483648)){_3Q=[1,I_fromInt(-2147483648)];continue;}else{var _3U=E(_3R);if(!_3U[0]){return [0,_3T%_3U[1]];}else{_3Q=[1,I_fromInt(_3T)];_3R=_3U;continue;}}}else{var _3V=_3S[1],_3W=E(_3R);return _3W[0]==0?[0,I_toInt(I_rem(_3V,I_fromInt(_3W[1])))]:[1,I_rem(_3V,_3W[1])];}}},_3X=function(_3Y,_3Z,_40){if(!E(_3t)){if(!B(_3j(B(_3P(_3Z,_3s)),_3r))){if(!B(_3j(_3Z,_3u))){if(!E(_3x)){return new F(function(){return (function(_41,_42,_43){while(1){if(!B(_3j(B(_3P(_42,_3s)),_3r))){if(!B(_3j(_42,_3u))){var _44=_41*_41,_45=B(_3H(B(_3y(_42,_3u)),_3v)),_46=_41*_43;_41=_44;_42=_45;_43=_46;continue;}else{return _41*_43;}}else{var _44=_41*_41,_45=B(_3H(_42,_3v));_41=_44;_42=_45;continue;}}})(_3Y*_3Y,B(_3H(B(_3y(_3Z,_3u)),_3v)),_3Y*_40);});}else{return E(_3i);}}else{return _3Y*_40;}}else{if(!E(_3x)){return new F(function(){return (function(_47,_48,_49){while(1){if(!B(_3j(B(_3P(_48,_3s)),_3r))){if(!B(_3j(_48,_3u))){var _4a=_47*_47,_4b=B(_3H(B(_3y(_48,_3u)),_3v)),_4c=_47*_49;_47=_4a;_48=_4b;_49=_4c;continue;}else{return _47*_49;}}else{var _4a=_47*_47,_4b=B(_3H(_48,_3v));_47=_4a;_48=_4b;continue;}}})(_3Y*_3Y,B(_3H(_3Z,_3v)),_40);});}else{return E(_3i);}}}else{return E(_3i);}},_4d=function(_4e,_4f){var _4g=E(_4e);if(!_4g[0]){var _4h=_4g[1],_4i=E(_4f);return _4i[0]==0?_4h<_4i[1]:I_compareInt(_4i[1],_4h)>0;}else{var _4j=_4g[1],_4k=E(_4f);return _4k[0]==0?I_compareInt(_4j,_4k[1])<0:I_compare(_4j,_4k[1])<0;}},_4l=new T(function(){return B(unCStr("Negative exponent"));}),_4m=new T(function(){return B(err(_4l));}),_4n=function(_4o,_4p){if(!B(_4d(_4p,_3w))){if(!B(_3j(_4p,_3w))){var _4q=E(_4o)[1];if(!E(_3t)){if(!B(_3j(B(_3P(_4p,_3s)),_3r))){if(!B(_3j(_4p,_3u))){if(!E(_3x)){return new F(function(){return _3X(_4q*_4q,B(_3H(B(_3y(_4p,_3u)),_3v)),_4q);});}else{return E(_3i);}}else{return E(_4q);}}else{if(!E(_3x)){return new F(function(){return (function(_4r,_4s){while(1){if(!B(_3j(B(_3P(_4s,_3s)),_3r))){if(!B(_3j(_4s,_3u))){return new F(function(){return _3X(_4r*_4r,B(_3H(B(_3y(_4s,_3u)),_3v)),_4r);});}else{return E(_4r);}}else{var _4t=_4r*_4r,_4u=B(_3H(_4s,_3v));_4r=_4t;_4s=_4u;continue;}}})(_4q*_4q,B(_3H(_4p,_3v)));});}else{return E(_3i);}}}else{return E(_3i);}}else{return 1;}}else{return E(_4m);}},_4v=[0,10],_4w=[0,10],_4x=new T(function(){return [0,B(_4n(_4w,_4v))];}),_4y=[1,_4x],_4z=[0,20],_4A=[0,2],_4B=[0,100],_4C=[0,0.1],_4D=[0,0.8],_4E=[0,_4D,_4C,_4B,_4A,_4z,_4y],_4F=[0,_2b,_2a,_29,_28,_4E],_4G=[0,_4F],_4H=new T(function(){return B(unCStr("color:red"));}),_4I=new T(function(){return B(unCStr("style"));}),_4J=[0,_4I,_4H],_4K=[1,_4J,_1g],_4L=[0,98],_4M=[1,_4L,_1g],_4N=function(_4O,_4P,_4Q,_){var _4R=E(_4P),_4S=B(A(_4O,[_4Q,_])),_4T=_4S,_4U=B(A(_d,[_t,_4T,_4R[1],_4R[2],_])),_4V=_4U;return _4T;},_4W=function(_4X,_4Y){while(1){var _4Z=(function(_50,_51){var _52=E(_51);if(!_52[0]){return E(_50);}else{_4X=function(_53,_){return new F(function(){return _4N(_50,_52[1],_53,_);});};_4Y=_52[2];return null;}})(_4X,_4Y);if(_4Z!=null){return _4Z;}}},_54=function(_55,_56,_){var _57=jsCreateElem(toJSStr(E(_55))),_58=_57,_59=jsAppendChild(_58,E(_56)[1]);return [0,_58];},_5a=function(_5b){return new F(function(){return _4W(function(_5c,_){var _5d=B(_54(_4M,_5c,_)),_5e=_5d,_5f=B(A(_5b,[_5e,_])),_5g=_5f;return _5e;},_4K);});},_5h=[0],_5i=function(_5j,_5k){var _5l=jsShowI(_5j),_5m=_5l;return new F(function(){return _2J(fromJSStr(_5m),_5k);});},_5n=[0,41],_5o=[0,40],_5p=function(_5q,_5r,_5s){if(_5r>=0){return new F(function(){return _5i(_5r,_5s);});}else{return _5q<=6?B(_5i(_5r,_5s)):[1,_5o,new T(function(){var _5t=jsShowI(_5r),_5u=_5t;return B(_2J(fromJSStr(_5u),[1,_5n,_5s]));})];}},_5v=[0,112],_5w=function(_5x){var _5y=new T(function(){return E(E(_5x)[2]);});return function(_5z,_){return [0,[1,_5v,new T(function(){return B(_2J(B(_5p(0,E(_5y)[1],_1g)),new T(function(){return E(E(_5x)[1]);},1)));})],new T(function(){var _5A=E(_5x);return [0,_5A[1],new T(function(){return [0,E(_5y)[1]+1|0];}),_5A[3],_5A[4],_5A[5],_5A[6],_5A[7]];})];};},_5B=new T(function(){return B(unCStr("id"));}),_5C=new T(function(){return B(unCStr("noid"));}),_5D=[0,0],_5E=false,_5F=2,_5G=[0],_5H=new T(function(){return B(unCStr("Dynamic"));}),_5I=new T(function(){return B(unCStr("Data.Dynamic"));}),_5J=new T(function(){return B(unCStr("base"));}),_5K=new T(function(){var _5L=hs_wordToWord64(628307645),_5M=_5L,_5N=hs_wordToWord64(949574464),_5O=_5N;return [0,_5M,_5O,[0,_5M,_5O,_5J,_5I,_5H],_1g];}),_5P=new T(function(){return B(unCStr("Haste.HPlay.View"));}),_5Q=new T(function(){return B(unCStr("hplayground-0.1.2.9"));}),_5R=new T(function(){return B(unCStr("EventData"));}),_5S=new T(function(){var _5T=hs_wordToWord64(1145008931),_5U=_5T,_5V=hs_wordToWord64(2687009104),_5W=_5V;return [0,_5U,_5W,[0,_5U,_5W,_5Q,_5P,_5R],_1g];}),_5X=[0],_5Y=new T(function(){return B(unCStr("OnLoad"));}),_5Z=[0,_5Y,_5X],_60=[0,_5S,_5Z],_61=[0,_5K,_60],_62=function(_){return _5h;},_63=function(_64,_){return _5h;},_65=[0,_62,_63],_66=[0,_1g,_5D,_5F,_65,_5E,_61,_5G],_67=function(_){var _=0,_68=newMVar(),_69=_68,_=putMVar(_69,_66);return [0,_69];},_6a=new T(function(){return B(_6(_67));}),_6b=function(_6c,_6d,_){var _6e=jsFind(toJSStr(E(_6d))),_6f=_6e,_6g=E(_6f);if(!_6g[0]){var _6h=E(_6a)[1],_6i=takeMVar(_6h),_6j=_6i,_6k=B(A(_6c,[_6j,_])),_6l=_6k,_6m=E(_6l),_=putMVar(_6h,_6m[2]);return E(_6m[1])[2];}else{var _6n=E(_6g[1]),_6o=jsClearChildren(_6n[1]),_6p=E(_6a)[1],_6q=takeMVar(_6p),_6r=_6q,_6s=B(A(_6c,[_6r,_])),_6t=_6s,_6u=E(_6t),_6v=E(_6u[1]),_=putMVar(_6p,_6u[2]),_6w=B(A(_6v[1],[_6n,_])),_6x=_6w;return _6v[2];}},_6y=new T(function(){return B(unCStr("span"));}),_6z=function(_6A,_6B,_6C,_){var _6D=jsCreateElem(toJSStr(E(_6y))),_6E=_6D,_6F=jsAppendChild(_6E,E(_6C)[1]),_6G=[0,_6E],_6H=B(A(_6A,[_6B,_6G,_])),_6I=_6H;return _6G;},_6J=function(_6K,_6L,_6M,_){var _6N=B(A(_5w,[_6M,_6M,_])),_6O=_6N,_6P=E(_6O),_6Q=_6P[1],_6R=E(_6P[2]),_6S=_6R[2],_6T=E(_6R[4]),_6U=B(A(_6K,[[0,_6R[1],_6S,_6R[3],[0,function(_){return new F(function(){return _6b(function(_6V,_){var _6W=B(A(_6K,[new T(function(){var _6X=E(_6V);return [0,_6X[1],_6S,_6X[3],_6X[4],_6X[5],_6X[6],_6X[7]];}),_])),_6Y=_6W;return [0,[0,_K,E(E(_6Y)[1])[2]],_6V];},_5C,_);});},function(_6Z,_){var _70=B(_6b(new T(function(){return B(A(_6L,[_6Z]));},1),_6Q,_)),_71=_70,_72=E(_71);return _72[0]==0?_5h:B(A(_6T[2],[_72[1],_]));}],_6R[5],_6R[6],_6R[7]],_])),_73=_6U,_74=E(_73),_75=_74[2],_76=E(_74[1]),_77=_76[1],_78=E(_76[2]);if(!_78[0]){return [0,[0,function(_79,_){var _7a=B(A(_77,[_79,_])),_7b=_7a;if(!E(E(_6M)[5])){var _7c=B(_6z(_Y,_K,_79,_)),_7d=_7c,_7e=B(A(_d,[_t,_7d,_5B,_6Q,_])),_7f=_7e;return _79;}else{return _79;}},_5h],new T(function(){var _7g=E(_75);return [0,_7g[1],_7g[2],_7g[3],_6T,_7g[5],_7g[6],_7g[7]];})];}else{var _7h=B(A(_6L,[_78[1],new T(function(){var _7i=E(_75);return [0,_7i[1],_7i[2],_7i[3],_6T,_7i[5],_7i[6],_7i[7]];}),_])),_7j=_7h,_7k=E(_7j),_7l=E(_7k[1]),_7m=_7l[1];return [0,[0,function(_7n,_){var _7o=B(A(_77,[_7n,_])),_7p=_7o;if(!E(E(_6M)[5])){var _7q=B(_6z(_Y,_7m,_7n,_)),_7r=_7q,_7s=B(A(_d,[_t,_7r,_5B,_6Q,_])),_7t=_7s;return _7n;}else{var _7u=B(A(_7m,[_7n,_])),_7v=_7u;return _7n;}},_7l[2]],_7k[2]];}},_7w=new T(function(){return B(unCStr("RouteShow"));}),_7x=new T(function(){return B(unCStr("RouteConfig"));}),_7y=function(_7z,_7A,_7B){while(1){var _7C=E(_7B);if(!_7C[0]){return [0,[0,_7z],_7A];}else{var _7D=_7C[2],_7E=E(_7C[1]),_7F=_7E[2],_7G=E(_7E[1])[1];if(_7z>=_7G){if(_7z!=_7G){_7B=_7D;continue;}else{_7z=_7G;_7A=_7F;_7B=_7D;continue;}}else{_7z=_7G;_7A=_7F;_7B=_7D;continue;}}}},_7H=function(_7I,_7J,_7K,_7L){var _7M=E(_7K);if(!_7M[0]){return E(_7J);}else{var _7N=E(_7L);if(!_7N[0]){return E(_7J);}else{return new F(function(){return A(_7I,[_7M[1],_7N[1],new T(function(){return B(_7H(_7I,_7J,_7M[2],_7N[2]));})]);});}}},_7O=new T(function(){return B(unCStr("List.maximumBy: empty list"));}),_7P=new T(function(){return B(err(_7O));}),_7Q=function(_7R,_7S){var _7T=E(_7S);return _7T[0]==0?[0]:[1,new T(function(){var _7U=B(_7H(function(_7V,_7W,_7X){return [1,[0,new T(function(){return B(A(_7R,[_7V]));}),_7W],_7X];},_1g,_7T,_7T));if(!_7U[0]){var _7Y=E(_7P);}else{var _7Z=E(_7U[1]),_80=_7Z[1],_81=_7Z[2],_82=E(_7U[2]);if(!_82[0]){var _83=[0,_80,_81];}else{var _84=_82[2],_85=E(_80)[1],_86=E(_82[1]),_87=_86[2],_88=E(_86[1])[1];if(_85>=_88){if(_85!=_88){var _89=B(_7y(_85,_81,_84)),_8a=[0,_89[1],_89[2]];}else{var _8b=B(_7y(_88,_87,_84)),_8a=[0,_8b[1],_8b[2]];}var _8c=_8a,_8d=_8c;}else{var _8e=B(_7y(_88,_87,_84)),_8d=[0,_8e[1],_8e[2]];}var _8f=_8d,_8g=_8f,_8h=_8g,_8i=_8h,_83=_8i;}var _8j=_83,_7Y=_8j;}return _7Y;})];},_8k=function(_8l){while(1){var _8m=(function(_8n){var _8o=E(_8n);if(!_8o[0]){return [0];}else{var _8p=_8o[2],_8q=E(_8o[1]);if(!_8q[0]){_8l=_8p;return null;}else{return [1,_8q[1],new T(function(){return B(_8k(_8p));})];}}})(_8l);if(_8m!=null){return _8m;}}},_8r=function(_8s,_8t,_8u){while(1){var _8v=E(_8u);if(!_8v[0]){return [0,[0,_8s],_8t];}else{var _8w=_8v[2],_8x=E(_8v[1]),_8y=_8x[2],_8z=E(_8x[1])[1];if(_8s>=_8z){if(_8s!=_8z){_8u=_8w;continue;}else{_8s=_8z;_8t=_8y;_8u=_8w;continue;}}else{_8s=_8z;_8t=_8y;_8u=_8w;continue;}}}},_8A=function(_8B,_8C){var _8D=E(_8C);return _8D[0]==0?[0]:[1,new T(function(){return B(A(_8B,[_8D[1]]));}),new T(function(){return B(_8A(_8B,_8D[2]));})];},_8E=function(_8F,_8G){var _8H=B(_8k(B(_8A(function(_8I){return new F(function(){return _7Q(_8F,_8I);});},_8G))));return _8H[0]==0?[0]:[1,new T(function(){var _8J=E(_8H[1]),_8K=_8J[1],_8L=_8J[2],_8M=E(_8H[2]);if(!_8M[0]){var _8N=[0,_8K,_8L];}else{var _8O=_8M[2],_8P=E(_8K)[1],_8Q=E(_8M[1]),_8R=_8Q[2],_8S=E(_8Q[1])[1];if(_8P>=_8S){if(_8P!=_8S){var _8T=B(_8r(_8P,_8L,_8O)),_8U=[0,_8T[1],_8T[2]];}else{var _8V=B(_8r(_8S,_8R,_8O)),_8U=[0,_8V[1],_8V[2]];}var _8W=_8U,_8X=_8W;}else{var _8Y=B(_8r(_8S,_8R,_8O)),_8X=[0,_8Y[1],_8Y[2]];}var _8Z=_8X,_90=_8Z,_91=_90,_92=_91,_8N=_92;}var _93=_8N;return _93;})];},_94=function(_95,_96,_97){while(1){var _98=E(_97);if(!_98[0]){return E(_95);}else{var _99=_95+E(_98[1])[1]*_96,_9a=_96/2;_97=_98[2];_95=_99;_96=_9a;continue;}}},_9b=[0,0],_9c=[0,41],_9d=[1,_9c,_1g],_9e=[0,40],_9f=function(_9g,_9h){return new F(function(){return _6(function(_){var _=0,_9i=B(A(_a,[toJSStr([1,_9e,new T(function(){return B(_2J(E(_9g)[4],_9d));})]),E(_9h),_])),_9j=_9i;return [0,_9j];});});},_9k=function(_9l){return new F(function(){return err(B(unAppCStr("Oops!  Entered absent arg ",new T(function(){return B(unCStr(_9l));}))));});},_9m=new T(function(){return B(_9k("ww_shYg{v} [lid] ghc-prim:GHC.Types.Int{(w) tc 3J}"));}),_9n=new T(function(){return B(_9k("ww_shYn{v} [lid] main:Genetic.Options.GeneticOptions{tc rjq}"));}),_9o=function(_9p,_9q,_9r,_9s){var _9t=E(_9s);if(!_9t[0]){return 1/Math.pow(_9q-B(_9f([0,_9p,_9m,[0,_9q],_9r,_9n],0))[1],2);}else{var _9u=E(_9p),_9v=Math.pow(2,_9u[1]-1);return 1/Math.pow(_9q-B(_9f([0,_9u,_9m,[0,_9q],_9r,_9n],B(_94(E(_9t[1])[1]*_9v,_9v/2,_9t[2]))))[1],2);}},_9w=function(_9x,_9y){var _9z=E(_9x);return [0,B(_9o(_9z[1],E(_9z[3])[1],_9z[4],_9y))];},_9A=function(_9B,_9C){var _9D=B(_8E(function(_9E){return new F(function(){return _9w(_9B,_9E);});},_9C));if(!_9D[0]){return [0];}else{var _9F=E(_9D[1]);return [1,[0,new T(function(){var _9G=E(_9F[2]);if(!_9G[0]){var _9H=E(_9b);}else{var _9I=Math.pow(2,E(E(_9B)[1])[1]-1),_9H=[0,B(_94(E(_9G[1])[1]*_9I,_9I/2,_9G[2]))];}return _9H;}),_9F[1]]];}},_9J=function(_){return new F(function(){return A(_a,["(function(){return md51(jsRand().toString());})",_]);});},_9K=function(_){return new F(function(){return _9J(_);});},_9L=[0,0],_9M=function(_9N,_){var _9O=B(_9K(_)),_9P=_9O;return [0,[0,_K,[1,[0,_5E,_9L,_1g,_5h,_9P]]],_9N];},_9Q=[1,_c],_9R=[0,_K,_9Q],_9S=function(_){var _=0,_9T=nMV(_1g),_9U=_9T;return [0,_9U];},_9V=new T(function(){return B(_6(_9S));}),_9W=function(_9X,_){var _=wMV(E(_9V)[1],_1g);return [0,_9R,_9X];},_9Y=new T(function(){return B(unCStr("RouteCalculate"));}),_9Z=new T(function(){return B(unAppCStr("invalid route in config state ",_9Y));}),_a0=function(_a1,_){return new F(function(){return _0(_9Z,_a1,_);});},_a2=new T(function(){return B(_5a(_a0));}),_a3=[0,_a2,_5h],_a4=function(_a5,_){return [0,_a3,_a5];},_a6=function(_a7,_a8,_){return [0,[0,_K,[1,[1,_a7]]],_a8];},_a9=function(_aa,_ab,_){return [0,[0,_K,[1,[0,_aa]]],_ab];},_ac=function(_ad,_ae,_af,_){var _ag=B(_6J(_ad,_a9,_af,_)),_ah=_ag,_ai=E(_ah),_aj=E(_ai[1]),_ak=B(_6J(_ae,_a6,_ai[2],_)),_al=_ak,_am=E(_al),_an=E(_am[1]);return [0,[0,function(_ao,_){var _ap=B(A(_aj[1],[_ao,_])),_aq=_ap,_ar=B(A(_an[1],[_ao,_])),_as=_ar;return _ao;},new T(function(){var _at=E(_aj[2]);return _at[0]==0?E(_an[2]):E(_at);})],_am[2]];},_au=function(_av){return new F(function(){return fromJSStr(E(_av)[1]);});},_aw=function(_ax){return function(_ay,_az){return new F(function(){return _0(new T(function(){return B(_au(_ax));}),_ay,_az);});};},_aA=new T(function(){return B(unCStr("class"));}),_aB=new T(function(){return B(unCStr("col-md-1"));}),_aC=new T(function(){return B(unCStr("col-md-11"));}),_aD=new T(function(){return B(unCStr("row-fluid"));}),_aE=new T(function(){return B(unCStr("style"));}),_aF=new T(function(){return [0,"\u041d\u0430\u0441\u0442\u0440\u043e\u0439\u043a\u0438 \u0444\u0443\u043d\u043a\u0446\u0438\u0438: "];}),_aG=new T(function(){return B(unCStr("font-size: 20px"));}),_aH=new T(function(){return B(unCStr("label"));}),_aI=function(_aJ,_aK,_aL,_){var _aM=jsCreateElem(toJSStr(E(_aH))),_aN=_aM,_aO=jsAppendChild(_aN,E(_aL)[1]),_aP=[0,_aN],_aQ=B(A(_aJ,[_aK,_aP,_])),_aR=_aQ;return _aP;},_aS=function(_aT,_){var _aU=B(_aI(_aw,_aF,_aT,_)),_aV=_aU,_aW=B(A(_d,[_t,_aV,_aE,_aG,_])),_aX=_aW;return _aV;},_aY=new T(function(){return B(unCStr("margin-top:10px"));}),_aZ=new T(function(){return [0,"\u041e\u0436\u0438\u0434\u0430\u0435\u043c\u043e\u0435 \u0437\u043d\u0430\u0447\u0435\u043d\u0438\u0435:"];}),_b0=function(_b1,_){return new F(function(){return _aI(_aw,_aZ,_b1,_);});},_b2=new T(function(){return B(unCStr("row"));}),_b3=new T(function(){return [0,"\u041d\u0430\u0441\u0442\u0440\u043e\u0439\u043a\u0438 \u044d\u0432\u043e\u043b\u044e\u0446\u0438\u0438:"];}),_b4=new T(function(){return B(unCStr("margin-top: 40px; font-size: 20px"));}),_b5=new T(function(){return [0,"\u0428\u0430\u043d\u0441 \u043c\u0443\u0442\u0430\u0446\u0438\u0438: "];}),_b6=function(_b1,_){return new F(function(){return _aI(_aw,_b5,_b1,_);});},_b7=new T(function(){return [0,"\u0432\u0435\u0440\u043e\u044f\u0442\u043d\u043e\u0441\u0442\u044c \u043d\u0435\u043a\u043e\u0440\u0440\u0435\u043a\u0442\u043d\u0430 [0, 1]"];}),_b8=[0,98],_b9=[1,_b8,_1g],_ba=function(_bb,_bc,_bd,_){var _be=jsCreateElem(toJSStr(_b9)),_bf=_be,_bg=jsAppendChild(_bf,E(_bd)[1]),_bh=[0,_bf],_bi=B(A(_bb,[_bc,_bh,_])),_bj=_bi;return _bh;},_bk=function(_b1,_){return new F(function(){return _ba(_aw,_b7,_b1,_);});},_bl=[1,_bk],_bm=function(_bn,_bo,_){return [0,new T(function(){var _bp=E(_bn)[1];if(_bp<0){var _bq=E(_bl);}else{var _bq=_bp>1?E(_bl):[0];}var _br=_bq,_bs=_br;return _bs;}),_bo];},_bt=new T(function(){return [0,"\u0427\u0430\u0441\u0442\u044c \u044d\u043b\u0438\u0442\u044b: "];}),_bu=function(_b1,_){return new F(function(){return _aI(_aw,_bt,_b1,_);});},_bv=new T(function(){return B(unCStr("div"));}),_bw=function(_bx,_by,_bz,_){var _bA=jsCreateElem(toJSStr(E(_bv))),_bB=_bA,_bC=jsAppendChild(_bB,E(_bz)[1]),_bD=[0,_bB],_bE=B(A(_bx,[_by,_bD,_])),_bF=_bE;return _bD;},_bG=function(_bH,_bI,_){var _bJ=B(_bw(_Y,_bH,_bI,_)),_bK=_bJ,_bL=B(A(_d,[_t,_bK,_aA,_b2,_])),_bM=_bL;return _bK;},_bN=new T(function(){return [0,"\u0434\u043e\u043b\u044f \u043d\u0435\u043a\u043e\u0440\u0440\u0435\u043a\u0442\u043d\u0430 [0, 1]"];}),_bO=function(_b1,_){return new F(function(){return _ba(_aw,_bN,_b1,_);});},_bP=[1,_bO],_bQ=function(_bR,_bS,_){return [0,new T(function(){var _bT=E(_bR)[1];if(_bT<0){var _bU=E(_bP);}else{var _bU=_bT>1?E(_bP):[0];}var _bV=_bU,_bW=_bV;return _bW;}),_bS];},_bX=new T(function(){return [0,"\u041c\u0430\u043a\u0441 \u043f\u043e\u043a\u043e\u043b\u0435\u043d\u0438\u0439: "];}),_bY=function(_b1,_){return new F(function(){return _aI(_aw,_bX,_b1,_);});},_bZ=new T(function(){return [0,"\u0434\u043e\u043b\u0436\u043d\u043e \u0431\u044b\u0442\u044c \u043f\u043e\u043b\u043e\u0436\u0438\u0442\u0435\u043b\u044c\u043d\u043e"];}),_c0=function(_b1,_){return new F(function(){return _ba(_aw,_bZ,_b1,_);});},_c1=[1,_c0],_c2=function(_c3,_c4,_){return [0,new T(function(){return E(_c3)[1]<=0?E(_c1):[0];}),_c4];},_c5=new T(function(){return [0,"\u0427\u0438\u0441\u043b\u043e \u043f\u043e\u043f\u0443\u043b\u044f\u0446\u0438\u0439: "];}),_c6=function(_b1,_){return new F(function(){return _aI(_aw,_c5,_b1,_);});},_c7=new T(function(){return B(unCStr("col-md-6"));}),_c8=new T(function(){return [0,"\u0427\u0438\u0441\u043b\u043e \u0438\u043d\u0434\u0438\u0432\u0438\u0434\u043e\u0432 \u0432 \u043f\u043e\u043f\u0443\u043b\u044f\u0446\u0438\u0438: "];}),_c9=function(_b1,_){return new F(function(){return _aI(_aw,_c8,_b1,_);});},_ca=function(_cb,_cc,_){return [0,[0,_K,[1,[1,_cb]]],_cc];},_cd=new T(function(){return [0,"\u041e\u0436\u0438\u0434\u0430\u0435\u043c\u044b\u0439 \u0444\u0438\u0442\u043d\u0435\u0441: "];}),_ce=function(_b1,_){return new F(function(){return _aI(_aw,_cd,_b1,_);});},_cf=new T(function(){return [0,"\u0424\u0443\u043d\u043a\u0446\u0438\u044f: "];}),_cg=function(_ch,_){var _ci=B(_aI(_aw,_cf,_ch,_)),_cj=_ci,_ck=B(A(_d,[_t,_cj,_aE,_b4,_])),_cl=_ck;return _cj;},_cm=new T(function(){return B(unCStr("rows"));}),_cn=[0,54],_co=[1,_cn,_1g],_cp=new T(function(){return B(unCStr("cols"));}),_cq=new T(function(){return B(unCStr("60"));}),_cr=function(_cs,_ct,_){var _cu=E(_cs);if(!_cu[0]){return _ct;}else{var _cv=B(A(_cu[1],[_ct,_])),_cw=_cv,_cx=B(_cr(_cu[2],_ct,_)),_cy=_cx;return _ct;}},_cz=new T(function(){return B(unCStr("row"));}),_cA=new T(function(){return B(unCStr("class"));}),_cB=function(_cC){return function(_ay,_az){return new F(function(){return _0(new T(function(){return B(_au(_cC));}),_ay,_az);});};},_cD=function(_cE,_cF,_cG){return function(_cH,_){var _cI=B(_bw(_Y,function(_cJ,_){return new F(function(){return _cr([1,function(_cK,_){var _cL=B(_bw(_Y,function(_cM,_){return new F(function(){return _aI(_cB,new T(function(){return [0,toJSStr(E(_cF))];}),_cM,_);});},_cK,_)),_cN=_cL,_cO=B(A(_d,[_t,_cN,_cA,new T(function(){return B(unAppCStr("col-md-",new T(function(){return B(_5p(0,E(_cE)[1],_1g));})));}),_])),_cP=_cO;return _cN;},[1,function(_cQ,_){var _cR=B(_bw(_cB,new T(function(){return [0,toJSStr(E(_cG))];}),_cQ,_)),_cS=_cR,_cT=B(A(_d,[_t,_cS,_cA,new T(function(){return B(unAppCStr("col-md-",new T(function(){return B(_5p(0,12-E(_cE)[1]|0,_1g));})));}),_])),_cU=_cT;return _cS;},_1g]],_cJ,_);});},_cH,_)),_cV=_cI,_cW=B(A(_d,[_t,_cV,_cA,_cz,_])),_cX=_cW;return _cV;};},_cY=[0,2],_cZ=new T(function(){return B(unCStr("x:"));}),_d0=new T(function(){return B(unCStr("\u0447\u0438\u0441\u043b\u043e \u0441 \u043f\u043b\u0430\u0432\u0430\u044e\u0449\u0435\u0439 \u0437\u0430\u043f\u044f\u0442\u043e\u0439, \u0430\u0440\u0433\u0443\u043c\u0435\u043d\u0442 \u0444\u0443\u043d\u043a\u0446\u0438\u0438"));}),_d1=new T(function(){return B(_cD(_cY,_cZ,_d0));}),_d2=[1,_d1,_1g],_d3=function(_d4,_){return new F(function(){return _cr(_d2,_d4,_);});},_d5=new T(function(){return B(unCStr("\u041f\u043e\u044f\u0441\u043d\u0435\u043d\u0438\u044f \u043a \u043f\u0430\u0440\u0430\u043c\u0435\u0442\u0440\u0430\u043c:"));}),_d6=new T(function(){return B(unCStr("h3"));}),_d7=function(_d8,_d9,_da,_){var _db=jsCreateElem(toJSStr(E(_d6))),_dc=_db,_dd=jsAppendChild(_dc,E(_da)[1]),_de=[0,_dc],_df=B(A(_d8,[_d9,_de,_])),_dg=_df;return _de;},_dh=new T(function(){return B(unCStr("panel panel-default"));}),_di=new T(function(){return B(unCStr("panel-heading"));}),_dj=new T(function(){return B(unCStr("panel-title"));}),_dk=new T(function(){return B(unCStr("panel-body"));}),_dl=function(_dm,_dn){return function(_do,_){var _dp=B(_bw(_Y,function(_dq,_){return new F(function(){return _cr([1,function(_dr,_){var _ds=B(_bw(_Y,function(_dt,_){var _du=B(_d7(_cB,new T(function(){return [0,toJSStr(E(_dm))];}),_dt,_)),_dv=_du,_dw=B(A(_d,[_t,_dv,_cA,_dj,_])),_dx=_dw;return _dv;},_dr,_)),_dy=_ds,_dz=B(A(_d,[_t,_dy,_cA,_di,_])),_dA=_dz;return _dy;},[1,function(_dB,_){var _dC=B(_bw(_Y,_dn,_dB,_)),_dD=_dC,_dE=B(A(_d,[_t,_dD,_cA,_dk,_])),_dF=_dE;return _dD;},_1g]],_dq,_);});},_do,_)),_dG=_dp,_dH=B(A(_d,[_t,_dG,_cA,_dh,_])),_dI=_dH;return _dG;};},_dJ=new T(function(){return B(_dl(_d5,_d3));}),_dK=[8,_],_dL=[13,_],_dM=new T(function(){return B(unCStr("wheel"));}),_dN=new T(function(){return B(unCStr("mouseout"));}),_dO=new T(function(){return B(unCStr("mouseover"));}),_dP=new T(function(){return B(unCStr("mousemove"));}),_dQ=new T(function(){return B(unCStr("blur"));}),_dR=new T(function(){return B(unCStr("focus"));}),_dS=new T(function(){return B(unCStr("change"));}),_dT=new T(function(){return B(unCStr("unload"));}),_dU=new T(function(){return B(unCStr("load"));}),_dV=new T(function(){return B(unCStr("submit"));}),_dW=new T(function(){return B(unCStr("keydown"));}),_dX=new T(function(){return B(unCStr("keyup"));}),_dY=new T(function(){return B(unCStr("keypress"));}),_dZ=new T(function(){return B(unCStr("mouseup"));}),_e0=new T(function(){return B(unCStr("mousedown"));}),_e1=new T(function(){return B(unCStr("dblclick"));}),_e2=new T(function(){return B(unCStr("click"));}),_e3=function(_e4){switch(E(_e4)[0]){case 0:return E(_dU);case 1:return E(_dT);case 2:return E(_dS);case 3:return E(_dR);case 4:return E(_dQ);case 5:return E(_dP);case 6:return E(_dO);case 7:return E(_dN);case 8:return E(_e2);case 9:return E(_e1);case 10:return E(_e0);case 11:return E(_dZ);case 12:return E(_dY);case 13:return E(_dX);case 14:return E(_dW);case 15:return E(_dV);default:return E(_dM);}},_e5=new T(function(){return B(unCStr("Control.Exception.Base"));}),_e6=new T(function(){return B(unCStr("base"));}),_e7=new T(function(){return B(unCStr("PatternMatchFail"));}),_e8=new T(function(){var _e9=hs_wordToWord64(18445595),_ea=_e9,_eb=hs_wordToWord64(52003073),_ec=_eb;return [0,_ea,_ec,[0,_ea,_ec,_e6,_e5,_e7],_1g];}),_ed=function(_ee){return E(_e8);},_ef=function(_eg){var _eh=E(_eg);return new F(function(){return _2o(B(_2m(_eh[1])),_ed,_eh[2]);});},_ei=function(_ej){return E(E(_ej)[1]);},_ek=function(_el,_em){return new F(function(){return _2J(E(_el)[1],_em);});},_en=function(_eo,_ep){return new F(function(){return _2Z(_ek,_eo,_ep);});},_eq=function(_er,_es,_et){return new F(function(){return _2J(E(_es)[1],_et);});},_eu=[0,_eq,_ei,_en],_ev=new T(function(){return [0,_ed,_eu,_ew,_ef];}),_ew=function(_ex){return [0,_ev,_ex];},_ey=new T(function(){return B(unCStr("Non-exhaustive patterns in"));}),_ez=function(_eA,_eB){return new F(function(){return die(new T(function(){return B(A(_eB,[_eA]));}));});},_eC=function(_eD,_eE){var _eF=E(_eE);if(!_eF[0]){return [0,_1g,_1g];}else{var _eG=_eF[1];if(!B(A(_eD,[_eG]))){return [0,_1g,_eF];}else{var _eH=new T(function(){var _eI=B(_eC(_eD,_eF[2]));return [0,_eI[1],_eI[2]];});return [0,[1,_eG,new T(function(){return E(E(_eH)[1]);})],new T(function(){return E(E(_eH)[2]);})];}}},_eJ=[0,32],_eK=[0,10],_eL=[1,_eK,_1g],_eM=function(_eN){return E(E(_eN)[1])==124?false:true;},_eO=function(_eP,_eQ){var _eR=B(_eC(_eM,B(unCStr(_eP)))),_eS=_eR[1],_eT=function(_eU,_eV){return new F(function(){return _2J(_eU,new T(function(){return B(unAppCStr(": ",new T(function(){return B(_2J(_eQ,new T(function(){return B(_2J(_eV,_eL));},1)));})));},1));});},_eW=E(_eR[2]);if(!_eW[0]){return new F(function(){return _eT(_eS,_1g);});}else{return E(E(_eW[1])[1])==124?B(_eT(_eS,[1,_eJ,_eW[2]])):B(_eT(_eS,_1g));}},_eX=function(_eY){return new F(function(){return _ez([0,new T(function(){return B(_eO(_eY,_ey));})],_ew);});},_eZ=new T(function(){return B(_eX("src/Haste/HPlay/View.hs:(1065,9)-(1099,63)|case"));}),_f0=[0,_dU,_5X],_f1=[0,_5S,_f0],_f2=[0,_dT,_5X],_f3=[0,_5S,_f2],_f4=[0,_dS,_5X],_f5=[0,_5S,_f4],_f6=[0,_dR,_5X],_f7=[0,_5S,_f6],_f8=[0,_dQ,_5X],_f9=[0,_5S,_f8],_fa=[3],_fb=[0,_dN,_fa],_fc=[0,_5S,_fb],_fd=function(_fe,_ff){switch(E(_fe)[0]){case 0:return function(_){var _fg=E(_6a)[1],_fh=takeMVar(_fg),_fi=_fh,_=putMVar(_fg,new T(function(){var _fj=E(_fi);return [0,_fj[1],_fj[2],_fj[3],_fj[4],_fj[5],_f1,_fj[7]];}));return new F(function(){return A(_ff,[_]);});};case 1:return function(_){var _fk=E(_6a)[1],_fl=takeMVar(_fk),_fm=_fl,_=putMVar(_fk,new T(function(){var _fn=E(_fm);return [0,_fn[1],_fn[2],_fn[3],_fn[4],_fn[5],_f3,_fn[7]];}));return new F(function(){return A(_ff,[_]);});};case 2:return function(_){var _fo=E(_6a)[1],_fp=takeMVar(_fo),_fq=_fp,_=putMVar(_fo,new T(function(){var _fr=E(_fq);return [0,_fr[1],_fr[2],_fr[3],_fr[4],_fr[5],_f5,_fr[7]];}));return new F(function(){return A(_ff,[_]);});};case 3:return function(_){var _fs=E(_6a)[1],_ft=takeMVar(_fs),_fu=_ft,_=putMVar(_fs,new T(function(){var _fv=E(_fu);return [0,_fv[1],_fv[2],_fv[3],_fv[4],_fv[5],_f7,_fv[7]];}));return new F(function(){return A(_ff,[_]);});};case 4:return function(_){var _fw=E(_6a)[1],_fx=takeMVar(_fw),_fy=_fx,_=putMVar(_fw,new T(function(){var _fz=E(_fy);return [0,_fz[1],_fz[2],_fz[3],_fz[4],_fz[5],_f9,_fz[7]];}));return new F(function(){return A(_ff,[_]);});};case 5:return function(_fA){return function(_){var _fB=E(_6a)[1],_fC=takeMVar(_fB),_fD=_fC,_=putMVar(_fB,new T(function(){var _fE=E(_fD);return [0,_fE[1],_fE[2],_fE[3],_fE[4],_fE[5],[0,_5S,[0,_dP,[2,E(_fA)]]],_fE[7]];}));return new F(function(){return A(_ff,[_]);});};};case 6:return function(_fF){return function(_){var _fG=E(_6a)[1],_fH=takeMVar(_fG),_fI=_fH,_=putMVar(_fG,new T(function(){var _fJ=E(_fI);return [0,_fJ[1],_fJ[2],_fJ[3],_fJ[4],_fJ[5],[0,_5S,[0,_dO,[2,E(_fF)]]],_fJ[7]];}));return new F(function(){return A(_ff,[_]);});};};case 7:return function(_){var _fK=E(_6a)[1],_fL=takeMVar(_fK),_fM=_fL,_=putMVar(_fK,new T(function(){var _fN=E(_fM);return [0,_fN[1],_fN[2],_fN[3],_fN[4],_fN[5],_fc,_fN[7]];}));return new F(function(){return A(_ff,[_]);});};case 8:return function(_fO,_fP){return function(_){var _fQ=E(_6a)[1],_fR=takeMVar(_fQ),_fS=_fR,_=putMVar(_fQ,new T(function(){var _fT=E(_fS);return [0,_fT[1],_fT[2],_fT[3],_fT[4],_fT[5],[0,_5S,[0,_e2,[1,_fO,E(_fP)]]],_fT[7]];}));return new F(function(){return A(_ff,[_]);});};};case 9:return function(_fU,_fV){return function(_){var _fW=E(_6a)[1],_fX=takeMVar(_fW),_fY=_fX,_=putMVar(_fW,new T(function(){var _fZ=E(_fY);return [0,_fZ[1],_fZ[2],_fZ[3],_fZ[4],_fZ[5],[0,_5S,[0,_e1,[1,_fU,E(_fV)]]],_fZ[7]];}));return new F(function(){return A(_ff,[_]);});};};case 10:return function(_g0,_g1){return function(_){var _g2=E(_6a)[1],_g3=takeMVar(_g2),_g4=_g3,_=putMVar(_g2,new T(function(){var _g5=E(_g4);return [0,_g5[1],_g5[2],_g5[3],_g5[4],_g5[5],[0,_5S,[0,_e0,[1,_g0,E(_g1)]]],_g5[7]];}));return new F(function(){return A(_ff,[_]);});};};case 11:return function(_g6,_g7){return function(_){var _g8=E(_6a)[1],_g9=takeMVar(_g8),_ga=_g9,_=putMVar(_g8,new T(function(){var _gb=E(_ga);return [0,_gb[1],_gb[2],_gb[3],_gb[4],_gb[5],[0,_5S,[0,_dZ,[1,_g6,E(_g7)]]],_gb[7]];}));return new F(function(){return A(_ff,[_]);});};};case 12:return function(_gc,_){var _gd=E(_6a)[1],_ge=takeMVar(_gd),_gf=_ge,_=putMVar(_gd,new T(function(){var _gg=E(_gf);return [0,_gg[1],_gg[2],_gg[3],_gg[4],_gg[5],[0,_5S,[0,_dY,[4,_gc]]],_gg[7]];}));return new F(function(){return A(_ff,[_]);});};case 13:return function(_gh,_){var _gi=E(_6a)[1],_gj=takeMVar(_gi),_gk=_gj,_=putMVar(_gi,new T(function(){var _gl=E(_gk);return [0,_gl[1],_gl[2],_gl[3],_gl[4],_gl[5],[0,_5S,[0,_dX,[4,_gh]]],_gl[7]];}));return new F(function(){return A(_ff,[_]);});};case 14:return function(_gm,_){var _gn=E(_6a)[1],_go=takeMVar(_gn),_gp=_go,_=putMVar(_gn,new T(function(){var _gq=E(_gp);return [0,_gq[1],_gq[2],_gq[3],_gq[4],_gq[5],[0,_5S,[0,_dW,[4,_gm]]],_gq[7]];}));return new F(function(){return A(_ff,[_]);});};default:return E(_eZ);}},_gr=[0,_e3,_fd],_gs=new T(function(){return B(unCStr("size"));}),_gt=[0,50],_gu=[1,_gt,_1g],_gv=[0,_K,_5h],_gw=true,_gx=[1,_c],_gy=[0,_K,_gx],_gz=function(_gA){var _gB=new T(function(){return E(E(_gA)[2]);});return function(_ay,_az){return new F(function(){return _6J(function(_gC,_){return [0,_gy,new T(function(){var _gD=E(_gA);return [0,_gD[1],new T(function(){return [0,E(_gB)[1]+1|0];}),_gD[3],_gD[4],_gD[5],_gD[6],_gD[7]];})];},function(_gE,_gF,_){return [0,[0,_K,[1,[1,_5v,new T(function(){return B(_2J(B(_5p(0,E(_gB)[1],_1g)),new T(function(){return E(E(_gA)[1]);},1)));})]]],_gF];},_ay,_az);});};},_gG=function(_gH,_){return [0,[0,_K,[1,_gH]],_gH];},_gI=function(_53,_){return new F(function(){return _6J(_gG,_gz,_53,_);});},_gJ=new T(function(){return B(unCStr(" could be found!"));}),_gK=function(_gL){return new F(function(){return err(B(unAppCStr("No element with ID ",new T(function(){return B(_2J(_gL,_gJ));}))));});},_gM=function(_gN,_gO,_gP,_){var _gQ=B(_6J(_gI,function(_gR,_gS,_){return new F(function(){return _6J(function(_gT,_){return [0,[0,function(_gU,_){var _gV=B(_6z(_Y,_K,_gU,_)),_gW=_gV,_gX=B(A(_d,[_t,_gW,_5B,_gR,_])),_gY=_gX;return _gW;},_gx],_gT];},function(_gZ,_53,_){return new F(function(){return (function(_53,_){return new F(function(){return _6J(_gN,function(_h0){return function(_h1,_){var _h2=B(A(new T(function(){return B(A(_gO,[_h0]));}),[_h1,_])),_h3=_h2,_h4=E(_h3),_h5=_h4[2],_h6=E(_h4[1]);if(!_h6[0]){var _h7=E(_gR),_h8=jsFind(toJSStr(_h7)),_h9=_h8,_ha=E(_h9);if(!_ha[0]){return new F(function(){return _gK(_h7);});}else{var _hb=jsClearChildren(E(_ha[1])[1]);return [0,[0,_K,[1,_h0]],_h5];}}else{var _hc=E(_gR),_hd=jsFind(toJSStr(_hc)),_he=_hd,_hf=E(_he);if(!_hf[0]){return new F(function(){return _gK(_hc);});}else{var _hg=E(_hf[1]),_hh=jsClearChildren(_hg[1]),_hi=B(A(_5a,[_h6[1],_hg,_])),_hj=_hi;return [0,_gv,_h5];}}};},_53,_);});})(_53,_);});},_gS,_);});},new T(function(){var _hk=E(_gP);return [0,_hk[1],_hk[2],_hk[3],_hk[4],_gw,_hk[6],_hk[7]];}),_)),_hl=_gQ;return [0,new T(function(){return E(E(_hl)[1]);}),new T(function(){var _hm=E(E(_hl)[2]);return [0,_hm[1],_hm[2],_hm[3],_hm[4],new T(function(){return E(E(_gP)[5]);}),_hm[6],_hm[7]];})];},_hn=new T(function(){return B(unCStr("button"));}),_ho=function(_hp,_hq,_hr,_){var _hs=jsCreateElem(toJSStr(E(_hn))),_ht=_hs,_hu=jsAppendChild(_ht,E(_hr)[1]),_hv=[0,_ht],_hw=B(A(_hp,[_hq,_hv,_])),_hx=_hw;return _hv;},_hy=new T(function(){return B(unCStr("margin-right: 10px; margin-left: 10px; margin-top: 3px"));}),_hz=new T(function(){return B(unCStr("button"));}),_hA=new T(function(){return B(unCStr("type"));}),_hB=new T(function(){return B(unCStr("btn btn-primary"));}),_hC=new T(function(){return B(unCStr("Maybe.fromJust: Nothing"));}),_hD=new T(function(){return B(err(_hC));}),_hE=function(_hF,_hG,_hH,_){var _hI=B(A(_hF,[_hH,_])),_hJ=_hI,_hK=E(_hJ),_hL=E(_hK[1]);return [0,[0,function(_hM,_){var _hN=B(A(_hL[1],[_hM,_])),_hO=_hN,_hP=jsFind(toJSStr(E(_hG))),_hQ=_hP;return new T(function(){var _hR=E(_hQ);return _hR[0]==0?E(_hD):E(_hR[1]);});},_hL[2]],_hK[2]];},_hS=function(_hT,_hU){while(1){var _hV=E(_hT);if(!_hV[0]){return E(_hU)[0]==0?true:false;}else{var _hW=E(_hU);if(!_hW[0]){return false;}else{if(E(_hV[1])[1]!=E(_hW[1])[1]){return false;}else{_hT=_hV[2];_hU=_hW[2];continue;}}}}},_hX=function(_hY){return E(_5S);},_hZ=function(_i0,_i1,_i2,_i3){var _i4=B(A(_i0,[_])),_i5=hs_eqWord64(_i1,_i4[1]),_i6=_i5;if(!E(_i6)){return [0];}else{var _i7=hs_eqWord64(_i2,_i4[2]),_i8=_i7;return E(_i8)==0?[0]:[1,_i3];}},_i9=function(_ia,_){return [0,[0,_K,new T(function(){var _ib=E(E(_ia)[6]),_ic=E(_ib[1]);return B(_hZ(_hX,_ic[1],_ic[2],_ib[2]));})],_ia];},_id=new T(function(){return B(unCStr("Onload"));}),_ie=[0,_id,_5X],_if=[0,_5S,_ie],_ig=function(_ih,_ii,_){return [0,_gy,new T(function(){var _ij=E(_ih);return [0,_ij[1],_ij[2],_ij[3],_ij[4],_ij[5],_if,_ij[7]];})];},_ik=function(_53,_){return new F(function(){return _6J(_gG,_ig,_53,_);});},_il=[0,_K,_5h],_im=function(_in,_io,_){var _ip=B(A(_in,[new T(function(){var _iq=E(_io);return [0,_iq[1],_iq[2],_iq[3],_iq[4],_gw,_iq[6],_iq[7]];}),_])),_ir=_ip;return [0,new T(function(){return E(E(_ir)[1]);}),new T(function(){var _is=E(E(_ir)[2]);return [0,_is[1],_is[2],_is[3],_is[4],new T(function(){return E(E(_io)[5]);}),_is[6],_is[7]];})];},_it=function(_iu){return E(E(_iu)[2]);},_iv=function(_iw){return E(E(_iw)[1]);},_ix=function(_iy,_iz,_iA){return function(_iB,_){var _iC=B(A(_iz,[_iB,_])),_iD=_iC,_iE=E(_iD),_iF=E(_iE[1]);return [0,[0,function(_iG,_){var _iH=B(A(_iF[1],[_iG,_])),_iI=_iH,_iJ=E(_iI),_iK=jsSetCB(_iJ[1],E(new T(function(){return [0,toJSStr(B(A(_iv,[_iy,_iA])))];}))[1],E(new T(function(){return B(A(new T(function(){return B(_it(_iy));}),[_iA,function(_){var _iL=E(E(_iB)[4]),_iM=B(A(_iL[1],[_])),_iN=_iM,_iO=E(_iN);if(!_iO[0]){return _c;}else{var _iP=B(A(_iL[2],[_iO[1],_])),_iQ=_iP;return _c;}}]));}))),_iR=_iK;return _iJ;},_iF[2]],_iE[2]];};},_iS=function(_iT,_iU){return function(_ay,_az){return new F(function(){return _im(function(_53,_){return new F(function(){return _6J(_ik,function(_iV,_53,_){return new F(function(){return (function(_53,_){return new F(function(){return _6J(new T(function(){return B(_ix(_gr,function(_iW,_){return [0,[0,_iT,_gx],_iW];},_iU));}),function(_iX){return function(_53,_){return new F(function(){return _6J(_i9,function(_iY){var _iZ=E(_iY);return new F(function(){return (function(_j0,_j1){return function(_j2,_){return !E(new T(function(){return B(_hS(new T(function(){return B(_e3(_iU));}),_j0));}))?[0,_il,_j2]:[0,[0,_K,[1,[0,_j0,_j1]]],_j2];};})(_iZ[1],_iZ[2]);});},_53,_);});};},_53,_);});})(_53,_);});},_53,_);});},_ay,_az);});};},_j3=function(_j4,_j5){return function(_ay,_az){return new F(function(){return _im(function(_cM,_){return new F(function(){return _hE(function(_j6,_){return new F(function(){return _6J(new T(function(){return B(_iS(function(_j7,_){var _j8=B(_ho(_0,_j5,_j7,_)),_j9=_j8,_ja=B(A(_d,[_t,_j9,_5B,_j5,_])),_jb=_ja,_jc=B(A(_d,[_t,_j9,_cA,_hB,_])),_jd=_jc,_je=B(A(_d,[_t,_j9,_hA,_hz,_])),_jf=_je,_jg=B(A(_d,[_t,_j9,_17,_hy,_])),_jh=_jg;return _j9;},_dK));}),function(_ji,_jj,_){return [0,[0,_K,[1,_j4]],_jj];},_j6,_);});},_j5,_cM,_);});},_ay,_az);});};},_jk=new T(function(){return B(unCStr("text"));}),_jl=[0,43],_jm=[1,_jl,_1g],_jn=[0,45],_jo=[1,_jn,_1g],_jp=function(_jq,_jr,_js,_){var _jt=B(_54(_jq,_js,_)),_ju=_jt,_jv=B(A(_jr,[_ju,_])),_jw=_jv;return _ju;},_jx=new T(function(){return B(unCStr("()"));}),_jy=new T(function(){return B(unCStr("GHC.Tuple"));}),_jz=new T(function(){return B(unCStr("ghc-prim"));}),_jA=new T(function(){var _jB=hs_wordToWord64(2170319554),_jC=_jB,_jD=hs_wordToWord64(26914641),_jE=_jD;return [0,_jC,_jE,[0,_jC,_jE,_jz,_jy,_jx],_1g];}),_jF=function(_jG){return E(_jA);},_jH=new T(function(){return B(unCStr("PerchM"));}),_jI=new T(function(){return B(unCStr("Haste.Perch"));}),_jJ=new T(function(){return B(unCStr("haste-perch-0.1.0.8"));}),_jK=new T(function(){var _jL=hs_wordToWord64(3848245492),_jM=_jL,_jN=hs_wordToWord64(243906750),_jO=_jN;return [0,_jM,_jO,[0,_jM,_jO,_jJ,_jI,_jH],_1g];}),_jP=function(_jQ){return E(_jK);},_jR=function(_jS){var _jT=E(_jS);if(!_jT[0]){return [0];}else{var _jU=E(_jT[1]);return [1,[0,_jU[1],_jU[2]],new T(function(){return B(_jR(_jT[2]));})];}},_jV=function(_jW,_jX){var _jY=E(_jW);if(!_jY){return [0,_1g,_jX];}else{var _jZ=E(_jX);if(!_jZ[0]){return [0,_1g,_1g];}else{var _k0=new T(function(){var _k1=B(_jV(_jY-1|0,_jZ[2]));return [0,_k1[1],_k1[2]];});return [0,[1,_jZ[1],new T(function(){return E(E(_k0)[1]);})],new T(function(){return E(E(_k0)[2]);})];}}},_k2=[0,120],_k3=[0,48],_k4=function(_k5){var _k6=new T(function(){var _k7=B(_jV(8,new T(function(){var _k8=md5(toJSStr(E(_k5))),_k9=_k8;return fromJSStr(_k9);})));return [0,_k7[1],_k7[2]];}),_ka=parseInt([0,toJSStr([1,_k3,[1,_k2,new T(function(){return E(E(_k6)[1]);})]])]),_kb=_ka,_kc=new T(function(){var _kd=B(_jV(8,new T(function(){return E(E(_k6)[2]);})));return [0,_kd[1],_kd[2]];}),_ke=parseInt([0,toJSStr([1,_k3,[1,_k2,new T(function(){return E(E(_kc)[1]);})]])]),_kf=_ke,_kg=hs_mkWord64(_kb,_kf),_kh=_kg,_ki=parseInt([0,toJSStr([1,_k3,[1,_k2,new T(function(){return E(B(_jV(8,new T(function(){return E(E(_kc)[2]);})))[1]);})]])]),_kj=_ki,_kk=hs_mkWord64(_kj,_kj),_kl=_kk;return [0,_kh,_kl];},_km=function(_kn){var _ko=E(_kn);if(!_ko[0]){return [0];}else{return new F(function(){return _2J(_ko[1],new T(function(){return B(_km(_ko[2]));},1));});}},_kp=function(_kq,_kr){var _ks=jsShowI(_kq),_kt=_ks,_ku=md5(_kt),_kv=_ku;return new F(function(){return _2J(fromJSStr(_kv),new T(function(){var _kw=jsShowI(_kr),_kx=_kw,_ky=md5(_kx),_kz=_ky;return fromJSStr(_kz);},1));});},_kA=function(_kB){var _kC=E(_kB);return new F(function(){return _kp(_kC[1],_kC[2]);});},_kD=function(_kE,_kF){return function(_kG){return E(new T(function(){var _kH=B(A(_kE,[_])),_kI=E(_kH[3]),_kJ=_kI[1],_kK=_kI[2],_kL=B(_2J(_kH[4],[1,new T(function(){return B(A(_kF,[_]));}),_1g]));if(!_kL[0]){var _kM=[0,_kJ,_kK,_kI,_1g];}else{var _kN=B(_k4(new T(function(){return B(_km(B(_8A(_kA,[1,[0,_kJ,_kK],new T(function(){return B(_jR(_kL));})]))));},1))),_kM=[0,_kN[1],_kN[2],_kI,_kL];}var _kO=_kM,_kP=_kO;return _kP;}));};},_kQ=new T(function(){return B(_kD(_jP,_jF));}),_kR=new T(function(){return B(unCStr("value"));}),_kS=new T(function(){return B(unCStr("id"));}),_kT=new T(function(){return B(unCStr("onclick"));}),_kU=new T(function(){return B(unCStr("checked"));}),_kV=[0,_kU,_1g],_kW=[1,_kV,_1g],_kX=new T(function(){return B(unCStr("type"));}),_kY=new T(function(){return B(unCStr("input"));}),_kZ=function(_l0,_){return new F(function(){return _54(_kY,_l0,_);});},_l1=function(_l2,_l3,_l4,_l5,_l6){var _l7=new T(function(){var _l8=new T(function(){return B(_4W(_kZ,[1,[0,_kX,_l3],[1,[0,_kS,_l2],[1,[0,_kR,_l4],_1g]]]));});return !E(_l5)?E(_l8):B(_4W(_l8,_kW));}),_l9=E(_l6);return _l9[0]==0?E(_l7):B(_4W(_l7,[1,[0,_kT,_l9[1]],_1g]));},_la=new T(function(){return B(unCStr("href"));}),_lb=[0,97],_lc=[1,_lb,_1g],_ld=function(_le,_){return new F(function(){return _54(_lc,_le,_);});},_lf=function(_lg,_lh){return function(_li,_){var _lj=B(A(new T(function(){return B(_4W(_ld,[1,[0,_la,_lg],_1g]));}),[_li,_])),_lk=_lj,_ll=B(A(_lh,[_lk,_])),_lm=_ll;return _lk;};},_ln=function(_lo){return new F(function(){return _lf(_lo,function(_53,_){return new F(function(){return _0(_lo,_53,_);});});});},_lp=new T(function(){return B(unCStr("option"));}),_lq=function(_lr,_){return new F(function(){return _54(_lp,_lr,_);});},_ls=new T(function(){return B(unCStr("selected"));}),_lt=[0,_ls,_1g],_lu=[1,_lt,_1g],_lv=function(_lw,_lx,_ly){var _lz=new T(function(){return B(_4W(_lq,[1,[0,_kR,_lw],_1g]));});if(!E(_ly)){return function(_lA,_){var _lB=B(A(_lz,[_lA,_])),_lC=_lB,_lD=B(A(_lx,[_lC,_])),_lE=_lD;return _lC;};}else{return new F(function(){return _4W(function(_lF,_){var _lG=B(A(_lz,[_lF,_])),_lH=_lG,_lI=B(A(_lx,[_lH,_])),_lJ=_lI;return _lH;},_lu);});}},_lK=function(_lL,_lM){return new F(function(){return _lv(_lL,function(_53,_){return new F(function(){return _0(_lL,_53,_);});},_lM);});},_lN=new T(function(){return B(unCStr("method"));}),_lO=new T(function(){return B(unCStr("action"));}),_lP=new T(function(){return B(unCStr("UTF-8"));}),_lQ=new T(function(){return B(unCStr("acceptCharset"));}),_lR=[0,_lQ,_lP],_lS=new T(function(){return B(unCStr("form"));}),_lT=function(_lU,_){return new F(function(){return _54(_lS,_lU,_);});},_lV=function(_lW,_lX,_lY){return function(_lZ,_){var _m0=B(A(new T(function(){return B(_4W(_lT,[1,_lR,[1,[0,_lO,_lW],[1,[0,_lN,_lX],_1g]]]));}),[_lZ,_])),_m1=_m0,_m2=B(A(_lY,[_m1,_])),_m3=_m2;return _m1;};},_m4=new T(function(){return B(unCStr("select"));}),_m5=function(_m6,_){return new F(function(){return _54(_m4,_m6,_);});},_m7=function(_m8,_m9){return function(_ma,_){var _mb=B(A(new T(function(){return B(_4W(_m5,[1,[0,_kS,_m8],_1g]));}),[_ma,_])),_mc=_mb,_md=B(A(_m9,[_mc,_])),_me=_md;return _mc;};},_mf=new T(function(){return B(unCStr("textarea"));}),_mg=function(_mh,_){return new F(function(){return _54(_mf,_mh,_);});},_mi=function(_mj,_mk){return function(_ml,_){var _mm=B(A(new T(function(){return B(_4W(_mg,[1,[0,_kS,_mj],_1g]));}),[_ml,_])),_mn=_mm,_mo=B(_0(_mk,_mn,_)),_mp=_mo;return _mn;};},_mq=function(_mr,_ms,_){return new F(function(){return _cr(_mr,_ms,_);});},_mt=function(_mu,_mv,_mw,_){var _mx=B(A(_mu,[_mw,_])),_my=_mx,_mz=B(A(_mv,[_mw,_])),_mA=_mz;return _mw;},_mB=[0,_K,_mt,_mq],_mC=[0,_mB,_kQ,_0,_0,_jp,_5a,_lf,_ln,_l1,_mi,_m7,_lv,_lK,_lV,_4W],_mD=new T(function(){return B(unCStr("GHC.IO.Exception"));}),_mE=new T(function(){return B(unCStr("base"));}),_mF=new T(function(){return B(unCStr("IOException"));}),_mG=new T(function(){var _mH=hs_wordToWord64(4053623282),_mI=_mH,_mJ=hs_wordToWord64(3693590983),_mK=_mJ;return [0,_mI,_mK,[0,_mI,_mK,_mE,_mD,_mF],_1g];}),_mL=function(_mM){return E(_mG);},_mN=function(_mO){var _mP=E(_mO);return new F(function(){return _2o(B(_2m(_mP[1])),_mL,_mP[2]);});},_mQ=new T(function(){return B(unCStr(": "));}),_mR=[0,41],_mS=new T(function(){return B(unCStr(" ("));}),_mT=new T(function(){return B(unCStr("already exists"));}),_mU=new T(function(){return B(unCStr("does not exist"));}),_mV=new T(function(){return B(unCStr("protocol error"));}),_mW=new T(function(){return B(unCStr("failed"));}),_mX=new T(function(){return B(unCStr("invalid argument"));}),_mY=new T(function(){return B(unCStr("inappropriate type"));}),_mZ=new T(function(){return B(unCStr("hardware fault"));}),_n0=new T(function(){return B(unCStr("unsupported operation"));}),_n1=new T(function(){return B(unCStr("timeout"));}),_n2=new T(function(){return B(unCStr("resource vanished"));}),_n3=new T(function(){return B(unCStr("interrupted"));}),_n4=new T(function(){return B(unCStr("resource busy"));}),_n5=new T(function(){return B(unCStr("resource exhausted"));}),_n6=new T(function(){return B(unCStr("end of file"));}),_n7=new T(function(){return B(unCStr("illegal operation"));}),_n8=new T(function(){return B(unCStr("permission denied"));}),_n9=new T(function(){return B(unCStr("user error"));}),_na=new T(function(){return B(unCStr("unsatisified constraints"));}),_nb=new T(function(){return B(unCStr("system error"));}),_nc=function(_nd,_ne){switch(E(_nd)){case 0:return new F(function(){return _2J(_mT,_ne);});break;case 1:return new F(function(){return _2J(_mU,_ne);});break;case 2:return new F(function(){return _2J(_n4,_ne);});break;case 3:return new F(function(){return _2J(_n5,_ne);});break;case 4:return new F(function(){return _2J(_n6,_ne);});break;case 5:return new F(function(){return _2J(_n7,_ne);});break;case 6:return new F(function(){return _2J(_n8,_ne);});break;case 7:return new F(function(){return _2J(_n9,_ne);});break;case 8:return new F(function(){return _2J(_na,_ne);});break;case 9:return new F(function(){return _2J(_nb,_ne);});break;case 10:return new F(function(){return _2J(_mV,_ne);});break;case 11:return new F(function(){return _2J(_mW,_ne);});break;case 12:return new F(function(){return _2J(_mX,_ne);});break;case 13:return new F(function(){return _2J(_mY,_ne);});break;case 14:return new F(function(){return _2J(_mZ,_ne);});break;case 15:return new F(function(){return _2J(_n0,_ne);});break;case 16:return new F(function(){return _2J(_n1,_ne);});break;case 17:return new F(function(){return _2J(_n2,_ne);});break;default:return new F(function(){return _2J(_n3,_ne);});}},_nf=[0,125],_ng=new T(function(){return B(unCStr("{handle: "));}),_nh=function(_ni,_nj,_nk,_nl,_nm,_nn){var _no=new T(function(){var _np=new T(function(){return B(_nc(_nj,new T(function(){var _nq=E(_nl);return _nq[0]==0?E(_nn):B(_2J(_mS,new T(function(){return B(_2J(_nq,[1,_mR,_nn]));},1)));},1)));},1),_nr=E(_nk);return _nr[0]==0?E(_np):B(_2J(_nr,new T(function(){return B(_2J(_mQ,_np));},1)));},1),_ns=E(_nm);if(!_ns[0]){var _nt=E(_ni);if(!_nt[0]){return E(_no);}else{var _nu=E(_nt[1]);return _nu[0]==0?B(_2J(_ng,new T(function(){return B(_2J(_nu[1],[1,_nf,new T(function(){return B(_2J(_mQ,_no));})]));},1))):B(_2J(_ng,new T(function(){return B(_2J(_nu[1],[1,_nf,new T(function(){return B(_2J(_mQ,_no));})]));},1)));}}else{return new F(function(){return _2J(_ns[1],new T(function(){return B(_2J(_mQ,_no));},1));});}},_nv=function(_nw){var _nx=E(_nw);return new F(function(){return _nh(_nx[1],_nx[2],_nx[3],_nx[4],_nx[6],_1g);});},_ny=function(_nz,_nA){var _nB=E(_nz);return new F(function(){return _nh(_nB[1],_nB[2],_nB[3],_nB[4],_nB[6],_nA);});},_nC=function(_nD,_nE){return new F(function(){return _2Z(_ny,_nD,_nE);});},_nF=function(_nG,_nH,_nI){var _nJ=E(_nH);return new F(function(){return _nh(_nJ[1],_nJ[2],_nJ[3],_nJ[4],_nJ[6],_nI);});},_nK=[0,_nF,_nv,_nC],_nL=new T(function(){return [0,_mL,_nK,_nM,_mN];}),_nM=function(_nN){return [0,_nL,_nN];},_nO=7,_nP=function(_nQ){return [0,_5h,_nO,_1g,_nQ,_5h,_5h];},_nR=function(_nS,_){return new F(function(){return die(new T(function(){return B(_nM(new T(function(){return B(_nP(_nS));})));}));});},_nT=function(_nU,_){return new F(function(){return _nR(_nU,_);});},_nV=function(_nW,_){return new F(function(){return _nT(_nW,_);});},_nX=function(_nY,_){return new F(function(){return _nV(_nY,_);});},_nZ=function(_o0,_o1,_){var _o2=B(A(_o0,[_])),_o3=_o2;return new F(function(){return A(_o1,[_o3,_]);});},_o4=function(_o5,_o6,_){var _o7=B(A(_o5,[_])),_o8=_o7;return new F(function(){return A(_o6,[_]);});},_o9=[0,_nZ,_o4,_K,_nX],_oa=[0,_o9,_t],_ob=new T(function(){return B(_eX("Text/ParserCombinators/ReadP.hs:(134,3)-(157,60)|function mplus"));}),_oc=function(_od,_oe){while(1){var _of=(function(_og,_oh){var _oi=E(_og);switch(_oi[0]){case 0:var _oj=E(_oh);if(!_oj[0]){return [0];}else{_od=B(A(_oi[1],[_oj[1]]));_oe=_oj[2];return null;}break;case 1:var _ok=B(A(_oi[1],[_oh])),_ol=_oh;_od=_ok;_oe=_ol;return null;case 2:return [0];case 3:return [1,[0,_oi[1],_oh],new T(function(){return B(_oc(_oi[2],_oh));})];default:return E(_oi[1]);}})(_od,_oe);if(_of!=null){return _of;}}},_om=function(_on,_oo){var _op=function(_oq){var _or=E(_oo);if(_or[0]==3){return [3,_or[1],new T(function(){return B(_om(_on,_or[2]));})];}else{var _os=E(_on);if(_os[0]==2){return E(_or);}else{var _ot=E(_or);if(_ot[0]==2){return E(_os);}else{var _ou=function(_ov){var _ow=E(_ot);if(_ow[0]==4){return [1,function(_ox){return [4,new T(function(){return B(_2J(B(_oc(_os,_ox)),_ow[1]));})];}];}else{var _oy=E(_os);if(_oy[0]==1){var _oz=_oy[1],_oA=E(_ow);return _oA[0]==0?[1,function(_oB){return new F(function(){return _om(B(A(_oz,[_oB])),_oA);});}]:[1,function(_oC){return new F(function(){return _om(B(A(_oz,[_oC])),new T(function(){return B(A(_oA[1],[_oC]));}));});}];}else{var _oD=E(_ow);return _oD[0]==0?E(_ob):[1,function(_oE){return new F(function(){return _om(_oy,new T(function(){return B(A(_oD[1],[_oE]));}));});}];}}},_oF=E(_os);switch(_oF[0]){case 1:var _oG=E(_ot);if(_oG[0]==4){return [1,function(_oH){return [4,new T(function(){return B(_2J(B(_oc(B(A(_oF[1],[_oH])),_oH)),_oG[1]));})];}];}else{return new F(function(){return _ou(_);});}break;case 4:var _oI=_oF[1],_oJ=E(_ot);switch(_oJ[0]){case 0:return [1,function(_oK){return [4,new T(function(){return B(_2J(_oI,new T(function(){return B(_oc(_oJ,_oK));},1)));})];}];case 1:return [1,function(_oL){return [4,new T(function(){return B(_2J(_oI,new T(function(){return B(_oc(B(A(_oJ[1],[_oL])),_oL));},1)));})];}];default:return [4,new T(function(){return B(_2J(_oI,_oJ[1]));})];}break;default:return new F(function(){return _ou(_);});}}}}},_oM=E(_on);switch(_oM[0]){case 0:var _oN=E(_oo);if(!_oN[0]){return [0,function(_oO){return new F(function(){return _om(B(A(_oM[1],[_oO])),new T(function(){return B(A(_oN[1],[_oO]));}));});}];}else{return new F(function(){return _op(_);});}break;case 3:return [3,_oM[1],new T(function(){return B(_om(_oM[2],_oo));})];default:return new F(function(){return _op(_);});}},_oP=[0,41],_oQ=[1,_oP,_1g],_oR=[0,40],_oS=[1,_oR,_1g],_oT=function(_oU,_oV){return E(_oU)[1]!=E(_oV)[1];},_oW=function(_oX,_oY){return E(_oX)[1]==E(_oY)[1];},_oZ=[0,_oW,_oT],_p0=function(_p1,_p2){while(1){var _p3=E(_p1);if(!_p3[0]){return E(_p2)[0]==0?true:false;}else{var _p4=E(_p2);if(!_p4[0]){return false;}else{if(E(_p3[1])[1]!=E(_p4[1])[1]){return false;}else{_p1=_p3[2];_p2=_p4[2];continue;}}}}},_p5=function(_p6,_p7){return !B(_p0(_p6,_p7))?true:false;},_p8=[0,_p0,_p5],_p9=function(_pa,_pb){var _pc=E(_pa);switch(_pc[0]){case 0:return [0,function(_pd){return new F(function(){return _p9(B(A(_pc[1],[_pd])),_pb);});}];case 1:return [1,function(_pe){return new F(function(){return _p9(B(A(_pc[1],[_pe])),_pb);});}];case 2:return [2];case 3:return new F(function(){return _om(B(A(_pb,[_pc[1]])),new T(function(){return B(_p9(_pc[2],_pb));}));});break;default:var _pf=function(_pg){var _ph=E(_pg);if(!_ph[0]){return [0];}else{var _pi=E(_ph[1]);return new F(function(){return _2J(B(_oc(B(A(_pb,[_pi[1]])),_pi[2])),new T(function(){return B(_pf(_ph[2]));},1));});}},_pj=B(_pf(_pc[1]));return _pj[0]==0?[2]:[4,_pj];}},_pk=[2],_pl=function(_pm){return [3,_pm,_pk];},_pn=function(_po,_pp){var _pq=E(_po);if(!_pq){return new F(function(){return A(_pp,[_c]);});}else{return [0,function(_pr){return E(new T(function(){return B(_pn(_pq-1|0,_pp));}));}];}},_ps=function(_pt,_pu,_pv){return function(_pw){return new F(function(){return A(function(_px,_py,_pz){while(1){var _pA=(function(_pB,_pC,_pD){var _pE=E(_pB);switch(_pE[0]){case 0:var _pF=E(_pC);if(!_pF[0]){return E(_pu);}else{_px=B(A(_pE[1],[_pF[1]]));_py=_pF[2];var _pG=_pD+1|0;_pz=_pG;return null;}break;case 1:var _pH=B(A(_pE[1],[_pC])),_pI=_pC,_pG=_pD;_px=_pH;_py=_pI;_pz=_pG;return null;case 2:return E(_pu);case 3:return function(_pJ){return new F(function(){return _pn(_pD,function(_pK){return E(new T(function(){return B(_p9(_pE,_pJ));}));});});};default:return function(_ay){return new F(function(){return _p9(_pE,_ay);});};}})(_px,_py,_pz);if(_pA!=null){return _pA;}}},[new T(function(){return B(A(_pt,[_pl]));}),_pw,0,_pv]);});};},_pL=function(_pM){return new F(function(){return A(_pM,[_1g]);});},_pN=function(_pO,_pP){var _pQ=function(_pR){var _pS=E(_pR);if(!_pS[0]){return E(_pL);}else{var _pT=_pS[1];return !B(A(_pO,[_pT]))?E(_pL):function(_pU){return [0,function(_pV){return E(new T(function(){return B(A(new T(function(){return B(_pQ(_pS[2]));}),[function(_pW){return new F(function(){return A(_pU,[[1,_pT,_pW]]);});}]));}));}];};}};return function(_pX){return new F(function(){return A(_pQ,[_pX,_pP]);});};},_pY=[6],_pZ=new T(function(){return B(unCStr("valDig: Bad base"));}),_q0=new T(function(){return B(err(_pZ));}),_q1=function(_q2,_q3){var _q4=function(_q5,_q6){var _q7=E(_q5);if(!_q7[0]){return function(_q8){return new F(function(){return A(_q8,[new T(function(){return B(A(_q6,[_1g]));})]);});};}else{var _q9=E(_q7[1])[1],_qa=function(_qb){return function(_qc){return [0,function(_qd){return E(new T(function(){return B(A(new T(function(){return B(_q4(_q7[2],function(_qe){return new F(function(){return A(_q6,[[1,_qb,_qe]]);});}));}),[_qc]));}));}];};};switch(E(E(_q2)[1])){case 8:if(48>_q9){return function(_qf){return new F(function(){return A(_qf,[new T(function(){return B(A(_q6,[_1g]));})]);});};}else{if(_q9>55){return function(_qg){return new F(function(){return A(_qg,[new T(function(){return B(A(_q6,[_1g]));})]);});};}else{return new F(function(){return _qa([0,_q9-48|0]);});}}break;case 10:if(48>_q9){return function(_qh){return new F(function(){return A(_qh,[new T(function(){return B(A(_q6,[_1g]));})]);});};}else{if(_q9>57){return function(_qi){return new F(function(){return A(_qi,[new T(function(){return B(A(_q6,[_1g]));})]);});};}else{return new F(function(){return _qa([0,_q9-48|0]);});}}break;case 16:if(48>_q9){if(97>_q9){if(65>_q9){return function(_qj){return new F(function(){return A(_qj,[new T(function(){return B(A(_q6,[_1g]));})]);});};}else{if(_q9>70){return function(_qk){return new F(function(){return A(_qk,[new T(function(){return B(A(_q6,[_1g]));})]);});};}else{return new F(function(){return _qa([0,(_q9-65|0)+10|0]);});}}}else{if(_q9>102){if(65>_q9){return function(_ql){return new F(function(){return A(_ql,[new T(function(){return B(A(_q6,[_1g]));})]);});};}else{if(_q9>70){return function(_qm){return new F(function(){return A(_qm,[new T(function(){return B(A(_q6,[_1g]));})]);});};}else{return new F(function(){return _qa([0,(_q9-65|0)+10|0]);});}}}else{return new F(function(){return _qa([0,(_q9-97|0)+10|0]);});}}}else{if(_q9>57){if(97>_q9){if(65>_q9){return function(_qn){return new F(function(){return A(_qn,[new T(function(){return B(A(_q6,[_1g]));})]);});};}else{if(_q9>70){return function(_qo){return new F(function(){return A(_qo,[new T(function(){return B(A(_q6,[_1g]));})]);});};}else{return new F(function(){return _qa([0,(_q9-65|0)+10|0]);});}}}else{if(_q9>102){if(65>_q9){return function(_qp){return new F(function(){return A(_qp,[new T(function(){return B(A(_q6,[_1g]));})]);});};}else{if(_q9>70){return function(_qq){return new F(function(){return A(_qq,[new T(function(){return B(A(_q6,[_1g]));})]);});};}else{return new F(function(){return _qa([0,(_q9-65|0)+10|0]);});}}}else{return new F(function(){return _qa([0,(_q9-97|0)+10|0]);});}}}else{return new F(function(){return _qa([0,_q9-48|0]);});}}break;default:return E(_q0);}}};return function(_qr){return new F(function(){return A(_q4,[_qr,_t,function(_qs){var _qt=E(_qs);return _qt[0]==0?[2]:B(A(_q3,[_qt]));}]);});};},_qu=[0,10],_qv=[0,1],_qw=[0,2147483647],_qx=function(_qy,_qz){while(1){var _qA=E(_qy);if(!_qA[0]){var _qB=_qA[1],_qC=E(_qz);if(!_qC[0]){var _qD=_qC[1],_qE=addC(_qB,_qD);if(!E(_qE[2])){return [0,_qE[1]];}else{_qy=[1,I_fromInt(_qB)];_qz=[1,I_fromInt(_qD)];continue;}}else{_qy=[1,I_fromInt(_qB)];_qz=_qC;continue;}}else{var _qF=E(_qz);if(!_qF[0]){_qy=_qA;_qz=[1,I_fromInt(_qF[1])];continue;}else{return [1,I_add(_qA[1],_qF[1])];}}}},_qG=new T(function(){return B(_qx(_qw,_qv));}),_qH=function(_qI){var _qJ=E(_qI);if(!_qJ[0]){var _qK=E(_qJ[1]);return _qK==(-2147483648)?E(_qG):[0, -_qK];}else{return [1,I_negate(_qJ[1])];}},_qL=[0,10],_qM=[0,0],_qN=function(_qO){return [0,_qO];},_qP=function(_qQ,_qR){while(1){var _qS=E(_qQ);if(!_qS[0]){var _qT=_qS[1],_qU=E(_qR);if(!_qU[0]){var _qV=_qU[1];if(!(imul(_qT,_qV)|0)){return [0,imul(_qT,_qV)|0];}else{_qQ=[1,I_fromInt(_qT)];_qR=[1,I_fromInt(_qV)];continue;}}else{_qQ=[1,I_fromInt(_qT)];_qR=_qU;continue;}}else{var _qW=E(_qR);if(!_qW[0]){_qQ=_qS;_qR=[1,I_fromInt(_qW[1])];continue;}else{return [1,I_mul(_qS[1],_qW[1])];}}}},_qX=function(_qY,_qZ,_r0){while(1){var _r1=E(_r0);if(!_r1[0]){return E(_qZ);}else{var _r2=B(_qx(B(_qP(_qZ,_qY)),B(_qN(E(_r1[1])[1]))));_r0=_r1[2];_qZ=_r2;continue;}}},_r3=function(_r4){var _r5=new T(function(){return B(_om(B(_om([0,function(_r6){return E(E(_r6)[1])==45?[1,B(_q1(_qu,function(_r7){return new F(function(){return A(_r4,[[1,new T(function(){return B(_qH(B(_qX(_qL,_qM,_r7))));})]]);});}))]:[2];}],[0,function(_r8){return E(E(_r8)[1])==43?[1,B(_q1(_qu,function(_r9){return new F(function(){return A(_r4,[[1,new T(function(){return B(_qX(_qL,_qM,_r9));})]]);});}))]:[2];}])),new T(function(){return [1,B(_q1(_qu,function(_ra){return new F(function(){return A(_r4,[[1,new T(function(){return B(_qX(_qL,_qM,_ra));})]]);});}))];})));});return new F(function(){return _om([0,function(_rb){return E(E(_rb)[1])==101?E(_r5):[2];}],[0,function(_rc){return E(E(_rc)[1])==69?E(_r5):[2];}]);});},_rd=function(_re){return new F(function(){return A(_re,[_5h]);});},_rf=function(_rg){return new F(function(){return A(_rg,[_5h]);});},_rh=function(_ri){return function(_rj){return E(E(_rj)[1])==46?[1,B(_q1(_qu,function(_rk){return new F(function(){return A(_ri,[[1,_rk]]);});}))]:[2];};},_rl=function(_rm){return [0,B(_rh(_rm))];},_rn=function(_ro){return new F(function(){return _q1(_qu,function(_rp){return [1,B(_ps(_rl,_rd,function(_rq){return [1,B(_ps(_r3,_rf,function(_rr){return new F(function(){return A(_ro,[[5,[1,_rp,_rq,_rr]]]);});}))];}))];});});},_rs=function(_rt){return [1,B(_rn(_rt))];},_ru=function(_rv){return E(E(_rv)[1]);},_rw=function(_rx,_ry,_rz){while(1){var _rA=E(_rz);if(!_rA[0]){return false;}else{if(!B(A(_ru,[_rx,_ry,_rA[1]]))){_rz=_rA[2];continue;}else{return true;}}}},_rB=new T(function(){return B(unCStr("!@#$%&*+./<=>?\\^|:-~"));}),_rC=function(_rD){return new F(function(){return _rw(_oZ,_rD,_rB);});},_rE=[0,8],_rF=[0,16],_rG=function(_rH){var _rI=function(_rJ){return new F(function(){return A(_rH,[[5,[0,_rE,_rJ]]]);});},_rK=function(_rL){return new F(function(){return A(_rH,[[5,[0,_rF,_rL]]]);});};return function(_rM){return E(E(_rM)[1])==48?E([0,function(_rN){switch(E(E(_rN)[1])){case 79:return [1,B(_q1(_rE,_rI))];case 88:return [1,B(_q1(_rF,_rK))];case 111:return [1,B(_q1(_rE,_rI))];case 120:return [1,B(_q1(_rF,_rK))];default:return [2];}}]):[2];};},_rO=function(_rP){return [0,B(_rG(_rP))];},_rQ=function(_rR){var _rS=new T(function(){return B(A(_rR,[_rE]));}),_rT=new T(function(){return B(A(_rR,[_rF]));});return function(_rU){switch(E(E(_rU)[1])){case 79:return E(_rS);case 88:return E(_rT);case 111:return E(_rS);case 120:return E(_rT);default:return [2];}};},_rV=function(_rW){return [0,B(_rQ(_rW))];},_rX=[0,92],_rY=function(_rZ){return new F(function(){return A(_rZ,[_qu]);});},_s0=function(_s1){return new F(function(){return err(B(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return B(_5p(9,_s1,_1g));}))));});},_s2=function(_s3){var _s4=E(_s3);return _s4[0]==0?E(_s4[1]):I_toInt(_s4[1]);},_s5=function(_s6,_s7){var _s8=E(_s6);if(!_s8[0]){var _s9=_s8[1],_sa=E(_s7);return _sa[0]==0?_s9<=_sa[1]:I_compareInt(_sa[1],_s9)>=0;}else{var _sb=_s8[1],_sc=E(_s7);return _sc[0]==0?I_compareInt(_sb,_sc[1])<=0:I_compare(_sb,_sc[1])<=0;}},_sd=function(_se){return [2];},_sf=function(_sg){var _sh=E(_sg);if(!_sh[0]){return E(_sd);}else{var _si=_sh[1],_sj=E(_sh[2]);return _sj[0]==0?E(_si):function(_sk){return new F(function(){return _om(B(A(_si,[_sk])),new T(function(){return B(A(new T(function(){return B(_sf(_sj));}),[_sk]));}));});};}},_sl=function(_sm){return [2];},_sn=function(_so,_sp){var _sq=function(_sr,_ss){var _st=E(_sr);if(!_st[0]){return function(_su){return new F(function(){return A(_su,[_so]);});};}else{var _sv=E(_ss);return _sv[0]==0?E(_sl):E(_st[1])[1]!=E(_sv[1])[1]?E(_sl):function(_sw){return [0,function(_sx){return E(new T(function(){return B(A(new T(function(){return B(_sq(_st[2],_sv[2]));}),[_sw]));}));}];};}};return function(_sy){return new F(function(){return A(_sq,[_so,_sy,_sp]);});};},_sz=new T(function(){return B(unCStr("SOH"));}),_sA=[0,1],_sB=function(_sC){return [1,B(_sn(_sz,function(_sD){return E(new T(function(){return B(A(_sC,[_sA]));}));}))];},_sE=new T(function(){return B(unCStr("SO"));}),_sF=[0,14],_sG=function(_sH){return [1,B(_sn(_sE,function(_sI){return E(new T(function(){return B(A(_sH,[_sF]));}));}))];},_sJ=function(_sK){return [1,B(_ps(_sB,_sG,_sK))];},_sL=new T(function(){return B(unCStr("NUL"));}),_sM=[0,0],_sN=function(_sO){return [1,B(_sn(_sL,function(_sP){return E(new T(function(){return B(A(_sO,[_sM]));}));}))];},_sQ=new T(function(){return B(unCStr("STX"));}),_sR=[0,2],_sS=function(_sT){return [1,B(_sn(_sQ,function(_sU){return E(new T(function(){return B(A(_sT,[_sR]));}));}))];},_sV=new T(function(){return B(unCStr("ETX"));}),_sW=[0,3],_sX=function(_sY){return [1,B(_sn(_sV,function(_sZ){return E(new T(function(){return B(A(_sY,[_sW]));}));}))];},_t0=new T(function(){return B(unCStr("EOT"));}),_t1=[0,4],_t2=function(_t3){return [1,B(_sn(_t0,function(_t4){return E(new T(function(){return B(A(_t3,[_t1]));}));}))];},_t5=new T(function(){return B(unCStr("ENQ"));}),_t6=[0,5],_t7=function(_t8){return [1,B(_sn(_t5,function(_t9){return E(new T(function(){return B(A(_t8,[_t6]));}));}))];},_ta=new T(function(){return B(unCStr("ACK"));}),_tb=[0,6],_tc=function(_td){return [1,B(_sn(_ta,function(_te){return E(new T(function(){return B(A(_td,[_tb]));}));}))];},_tf=new T(function(){return B(unCStr("BEL"));}),_tg=[0,7],_th=function(_ti){return [1,B(_sn(_tf,function(_tj){return E(new T(function(){return B(A(_ti,[_tg]));}));}))];},_tk=new T(function(){return B(unCStr("BS"));}),_tl=[0,8],_tm=function(_tn){return [1,B(_sn(_tk,function(_to){return E(new T(function(){return B(A(_tn,[_tl]));}));}))];},_tp=new T(function(){return B(unCStr("HT"));}),_tq=[0,9],_tr=function(_ts){return [1,B(_sn(_tp,function(_tt){return E(new T(function(){return B(A(_ts,[_tq]));}));}))];},_tu=new T(function(){return B(unCStr("LF"));}),_tv=[0,10],_tw=function(_tx){return [1,B(_sn(_tu,function(_ty){return E(new T(function(){return B(A(_tx,[_tv]));}));}))];},_tz=new T(function(){return B(unCStr("VT"));}),_tA=[0,11],_tB=function(_tC){return [1,B(_sn(_tz,function(_tD){return E(new T(function(){return B(A(_tC,[_tA]));}));}))];},_tE=new T(function(){return B(unCStr("FF"));}),_tF=[0,12],_tG=function(_tH){return [1,B(_sn(_tE,function(_tI){return E(new T(function(){return B(A(_tH,[_tF]));}));}))];},_tJ=new T(function(){return B(unCStr("CR"));}),_tK=[0,13],_tL=function(_tM){return [1,B(_sn(_tJ,function(_tN){return E(new T(function(){return B(A(_tM,[_tK]));}));}))];},_tO=new T(function(){return B(unCStr("SI"));}),_tP=[0,15],_tQ=function(_tR){return [1,B(_sn(_tO,function(_tS){return E(new T(function(){return B(A(_tR,[_tP]));}));}))];},_tT=new T(function(){return B(unCStr("DLE"));}),_tU=[0,16],_tV=function(_tW){return [1,B(_sn(_tT,function(_tX){return E(new T(function(){return B(A(_tW,[_tU]));}));}))];},_tY=new T(function(){return B(unCStr("DC1"));}),_tZ=[0,17],_u0=function(_u1){return [1,B(_sn(_tY,function(_u2){return E(new T(function(){return B(A(_u1,[_tZ]));}));}))];},_u3=new T(function(){return B(unCStr("DC2"));}),_u4=[0,18],_u5=function(_u6){return [1,B(_sn(_u3,function(_u7){return E(new T(function(){return B(A(_u6,[_u4]));}));}))];},_u8=new T(function(){return B(unCStr("DC3"));}),_u9=[0,19],_ua=function(_ub){return [1,B(_sn(_u8,function(_uc){return E(new T(function(){return B(A(_ub,[_u9]));}));}))];},_ud=new T(function(){return B(unCStr("DC4"));}),_ue=[0,20],_uf=function(_ug){return [1,B(_sn(_ud,function(_uh){return E(new T(function(){return B(A(_ug,[_ue]));}));}))];},_ui=new T(function(){return B(unCStr("NAK"));}),_uj=[0,21],_uk=function(_ul){return [1,B(_sn(_ui,function(_um){return E(new T(function(){return B(A(_ul,[_uj]));}));}))];},_un=new T(function(){return B(unCStr("SYN"));}),_uo=[0,22],_up=function(_uq){return [1,B(_sn(_un,function(_ur){return E(new T(function(){return B(A(_uq,[_uo]));}));}))];},_us=new T(function(){return B(unCStr("ETB"));}),_ut=[0,23],_uu=function(_uv){return [1,B(_sn(_us,function(_uw){return E(new T(function(){return B(A(_uv,[_ut]));}));}))];},_ux=new T(function(){return B(unCStr("CAN"));}),_uy=[0,24],_uz=function(_uA){return [1,B(_sn(_ux,function(_uB){return E(new T(function(){return B(A(_uA,[_uy]));}));}))];},_uC=new T(function(){return B(unCStr("EM"));}),_uD=[0,25],_uE=function(_uF){return [1,B(_sn(_uC,function(_uG){return E(new T(function(){return B(A(_uF,[_uD]));}));}))];},_uH=new T(function(){return B(unCStr("SUB"));}),_uI=[0,26],_uJ=function(_uK){return [1,B(_sn(_uH,function(_uL){return E(new T(function(){return B(A(_uK,[_uI]));}));}))];},_uM=new T(function(){return B(unCStr("ESC"));}),_uN=[0,27],_uO=function(_uP){return [1,B(_sn(_uM,function(_uQ){return E(new T(function(){return B(A(_uP,[_uN]));}));}))];},_uR=new T(function(){return B(unCStr("FS"));}),_uS=[0,28],_uT=function(_uU){return [1,B(_sn(_uR,function(_uV){return E(new T(function(){return B(A(_uU,[_uS]));}));}))];},_uW=new T(function(){return B(unCStr("GS"));}),_uX=[0,29],_uY=function(_uZ){return [1,B(_sn(_uW,function(_v0){return E(new T(function(){return B(A(_uZ,[_uX]));}));}))];},_v1=new T(function(){return B(unCStr("RS"));}),_v2=[0,30],_v3=function(_v4){return [1,B(_sn(_v1,function(_v5){return E(new T(function(){return B(A(_v4,[_v2]));}));}))];},_v6=new T(function(){return B(unCStr("US"));}),_v7=[0,31],_v8=function(_v9){return [1,B(_sn(_v6,function(_va){return E(new T(function(){return B(A(_v9,[_v7]));}));}))];},_vb=new T(function(){return B(unCStr("SP"));}),_vc=[0,32],_vd=function(_ve){return [1,B(_sn(_vb,function(_vf){return E(new T(function(){return B(A(_ve,[_vc]));}));}))];},_vg=new T(function(){return B(unCStr("DEL"));}),_vh=[0,127],_vi=function(_vj){return [1,B(_sn(_vg,function(_vk){return E(new T(function(){return B(A(_vj,[_vh]));}));}))];},_vl=[1,_vi,_1g],_vm=[1,_vd,_vl],_vn=[1,_v8,_vm],_vo=[1,_v3,_vn],_vp=[1,_uY,_vo],_vq=[1,_uT,_vp],_vr=[1,_uO,_vq],_vs=[1,_uJ,_vr],_vt=[1,_uE,_vs],_vu=[1,_uz,_vt],_vv=[1,_uu,_vu],_vw=[1,_up,_vv],_vx=[1,_uk,_vw],_vy=[1,_uf,_vx],_vz=[1,_ua,_vy],_vA=[1,_u5,_vz],_vB=[1,_u0,_vA],_vC=[1,_tV,_vB],_vD=[1,_tQ,_vC],_vE=[1,_tL,_vD],_vF=[1,_tG,_vE],_vG=[1,_tB,_vF],_vH=[1,_tw,_vG],_vI=[1,_tr,_vH],_vJ=[1,_tm,_vI],_vK=[1,_th,_vJ],_vL=[1,_tc,_vK],_vM=[1,_t7,_vL],_vN=[1,_t2,_vM],_vO=[1,_sX,_vN],_vP=[1,_sS,_vO],_vQ=[1,_sN,_vP],_vR=[1,_sJ,_vQ],_vS=new T(function(){return B(_sf(_vR));}),_vT=[0,1114111],_vU=[0,34],_vV=[0,39],_vW=function(_vX){var _vY=new T(function(){return B(A(_vX,[_tg]));}),_vZ=new T(function(){return B(A(_vX,[_tl]));}),_w0=new T(function(){return B(A(_vX,[_tq]));}),_w1=new T(function(){return B(A(_vX,[_tv]));}),_w2=new T(function(){return B(A(_vX,[_tA]));}),_w3=new T(function(){return B(A(_vX,[_tF]));}),_w4=new T(function(){return B(A(_vX,[_tK]));});return new F(function(){return _om([0,function(_w5){switch(E(E(_w5)[1])){case 34:return E(new T(function(){return B(A(_vX,[_vU]));}));case 39:return E(new T(function(){return B(A(_vX,[_vV]));}));case 92:return E(new T(function(){return B(A(_vX,[_rX]));}));case 97:return E(_vY);case 98:return E(_vZ);case 102:return E(_w3);case 110:return E(_w1);case 114:return E(_w4);case 116:return E(_w0);case 118:return E(_w2);default:return [2];}}],new T(function(){return B(_om([1,B(_ps(_rV,_rY,function(_w6){return [1,B(_q1(_w6,function(_w7){var _w8=B(_qX(new T(function(){return B(_qN(E(_w6)[1]));}),_qM,_w7));return !B(_s5(_w8,_vT))?[2]:B(A(_vX,[new T(function(){var _w9=B(_s2(_w8));if(_w9>>>0>1114111){var _wa=B(_s0(_w9));}else{var _wa=[0,_w9];}var _wb=_wa,_wc=_wb,_wd=_wc;return _wd;})]));}))];}))],new T(function(){return B(_om([0,function(_we){return E(E(_we)[1])==94?E([0,function(_wf){switch(E(E(_wf)[1])){case 64:return E(new T(function(){return B(A(_vX,[_sM]));}));case 65:return E(new T(function(){return B(A(_vX,[_sA]));}));case 66:return E(new T(function(){return B(A(_vX,[_sR]));}));case 67:return E(new T(function(){return B(A(_vX,[_sW]));}));case 68:return E(new T(function(){return B(A(_vX,[_t1]));}));case 69:return E(new T(function(){return B(A(_vX,[_t6]));}));case 70:return E(new T(function(){return B(A(_vX,[_tb]));}));case 71:return E(_vY);case 72:return E(_vZ);case 73:return E(_w0);case 74:return E(_w1);case 75:return E(_w2);case 76:return E(_w3);case 77:return E(_w4);case 78:return E(new T(function(){return B(A(_vX,[_sF]));}));case 79:return E(new T(function(){return B(A(_vX,[_tP]));}));case 80:return E(new T(function(){return B(A(_vX,[_tU]));}));case 81:return E(new T(function(){return B(A(_vX,[_tZ]));}));case 82:return E(new T(function(){return B(A(_vX,[_u4]));}));case 83:return E(new T(function(){return B(A(_vX,[_u9]));}));case 84:return E(new T(function(){return B(A(_vX,[_ue]));}));case 85:return E(new T(function(){return B(A(_vX,[_uj]));}));case 86:return E(new T(function(){return B(A(_vX,[_uo]));}));case 87:return E(new T(function(){return B(A(_vX,[_ut]));}));case 88:return E(new T(function(){return B(A(_vX,[_uy]));}));case 89:return E(new T(function(){return B(A(_vX,[_uD]));}));case 90:return E(new T(function(){return B(A(_vX,[_uI]));}));case 91:return E(new T(function(){return B(A(_vX,[_uN]));}));case 92:return E(new T(function(){return B(A(_vX,[_uS]));}));case 93:return E(new T(function(){return B(A(_vX,[_uX]));}));case 94:return E(new T(function(){return B(A(_vX,[_v2]));}));case 95:return E(new T(function(){return B(A(_vX,[_v7]));}));default:return [2];}}]):[2];}],new T(function(){return B(A(_vS,[_vX]));})));})));}));});},_wg=function(_wh){return new F(function(){return A(_wh,[_c]);});},_wi=function(_wj){var _wk=E(_wj);if(!_wk[0]){return E(_wg);}else{var _wl=_wk[2],_wm=E(E(_wk[1])[1]);switch(_wm){case 9:return function(_wn){return [0,function(_wo){return E(new T(function(){return B(A(new T(function(){return B(_wi(_wl));}),[_wn]));}));}];};case 10:return function(_wp){return [0,function(_wq){return E(new T(function(){return B(A(new T(function(){return B(_wi(_wl));}),[_wp]));}));}];};case 11:return function(_wr){return [0,function(_ws){return E(new T(function(){return B(A(new T(function(){return B(_wi(_wl));}),[_wr]));}));}];};case 12:return function(_wt){return [0,function(_wu){return E(new T(function(){return B(A(new T(function(){return B(_wi(_wl));}),[_wt]));}));}];};case 13:return function(_wv){return [0,function(_ww){return E(new T(function(){return B(A(new T(function(){return B(_wi(_wl));}),[_wv]));}));}];};case 32:return function(_wx){return [0,function(_wy){return E(new T(function(){return B(A(new T(function(){return B(_wi(_wl));}),[_wx]));}));}];};case 160:return function(_wz){return [0,function(_wA){return E(new T(function(){return B(A(new T(function(){return B(_wi(_wl));}),[_wz]));}));}];};default:var _wB=u_iswspace(_wm),_wC=_wB;return E(_wC)==0?E(_wg):function(_wD){return [0,function(_wE){return E(new T(function(){return B(A(new T(function(){return B(_wi(_wl));}),[_wD]));}));}];};}}},_wF=function(_wG){var _wH=new T(function(){return B(_wF(_wG));}),_wI=[1,function(_wJ){return new F(function(){return A(_wi,[_wJ,function(_wK){return E([0,function(_wL){return E(E(_wL)[1])==92?E(_wH):[2];}]);}]);});}];return new F(function(){return _om([0,function(_wM){return E(E(_wM)[1])==92?E([0,function(_wN){var _wO=E(E(_wN)[1]);switch(_wO){case 9:return E(_wI);case 10:return E(_wI);case 11:return E(_wI);case 12:return E(_wI);case 13:return E(_wI);case 32:return E(_wI);case 38:return E(_wH);case 160:return E(_wI);default:var _wP=u_iswspace(_wO),_wQ=_wP;return E(_wQ)==0?[2]:E(_wI);}}]):[2];}],[0,function(_wR){var _wS=E(_wR);return E(_wS[1])==92?E(new T(function(){return B(_vW(function(_wT){return new F(function(){return A(_wG,[[0,_wT,_gw]]);});}));})):B(A(_wG,[[0,_wS,_5E]]));}]);});},_wU=function(_wV,_wW){return new F(function(){return _wF(function(_wX){var _wY=E(_wX),_wZ=E(_wY[1]);if(E(_wZ[1])==34){if(!E(_wY[2])){return E(new T(function(){return B(A(_wW,[[1,new T(function(){return B(A(_wV,[_1g]));})]]));}));}else{return new F(function(){return _wU(function(_x0){return new F(function(){return A(_wV,[[1,_wZ,_x0]]);});},_wW);});}}else{return new F(function(){return _wU(function(_x1){return new F(function(){return A(_wV,[[1,_wZ,_x1]]);});},_wW);});}});});},_x2=new T(function(){return B(unCStr("_\'"));}),_x3=function(_x4){var _x5=u_iswalnum(_x4),_x6=_x5;return E(_x6)==0?B(_rw(_oZ,[0,_x4],_x2)):true;},_x7=function(_x8){return new F(function(){return _x3(E(_x8)[1]);});},_x9=new T(function(){return B(unCStr(",;()[]{}`"));}),_xa=new T(function(){return B(unCStr(".."));}),_xb=new T(function(){return B(unCStr("::"));}),_xc=new T(function(){return B(unCStr("->"));}),_xd=[0,64],_xe=[1,_xd,_1g],_xf=[0,126],_xg=[1,_xf,_1g],_xh=new T(function(){return B(unCStr("=>"));}),_xi=[1,_xh,_1g],_xj=[1,_xg,_xi],_xk=[1,_xe,_xj],_xl=[1,_xc,_xk],_xm=new T(function(){return B(unCStr("<-"));}),_xn=[1,_xm,_xl],_xo=[0,124],_xp=[1,_xo,_1g],_xq=[1,_xp,_xn],_xr=[1,_rX,_1g],_xs=[1,_xr,_xq],_xt=[0,61],_xu=[1,_xt,_1g],_xv=[1,_xu,_xs],_xw=[1,_xb,_xv],_xx=[1,_xa,_xw],_xy=function(_xz){return new F(function(){return _om([1,function(_xA){return E(_xA)[0]==0?E(new T(function(){return B(A(_xz,[_pY]));})):[2];}],new T(function(){return B(_om([0,function(_xB){return E(E(_xB)[1])==39?E([0,function(_xC){var _xD=E(_xC);switch(E(_xD[1])){case 39:return [2];case 92:return E(new T(function(){return B(_vW(function(_xE){return [0,function(_xF){return E(E(_xF)[1])==39?E(new T(function(){return B(A(_xz,[[0,_xE]]));})):[2];}];}));}));default:return [0,function(_xG){return E(E(_xG)[1])==39?E(new T(function(){return B(A(_xz,[[0,_xD]]));})):[2];}];}}]):[2];}],new T(function(){return B(_om([0,function(_xH){return E(E(_xH)[1])==34?E(new T(function(){return B(_wU(_t,_xz));})):[2];}],new T(function(){return B(_om([0,function(_xI){return !B(_rw(_oZ,_xI,_x9))?[2]:B(A(_xz,[[2,[1,_xI,_1g]]]));}],new T(function(){return B(_om([0,function(_xJ){return !B(_rw(_oZ,_xJ,_rB))?[2]:[1,B(_pN(_rC,function(_xK){var _xL=[1,_xJ,_xK];return !B(_rw(_p8,_xL,_xx))?B(A(_xz,[[4,_xL]])):B(A(_xz,[[2,_xL]]));}))];}],new T(function(){return B(_om([0,function(_xM){var _xN=E(_xM),_xO=_xN[1],_xP=u_iswalpha(_xO),_xQ=_xP;return E(_xQ)==0?E(_xO)==95?[1,B(_pN(_x7,function(_xR){return new F(function(){return A(_xz,[[3,[1,_xN,_xR]]]);});}))]:[2]:[1,B(_pN(_x7,function(_xS){return new F(function(){return A(_xz,[[3,[1,_xN,_xS]]]);});}))];}],new T(function(){return [1,B(_ps(_rO,_rs,_xz))];})));})));})));})));})));}));});},_xT=[0,0],_xU=function(_xV,_xW){return function(_xX){return new F(function(){return A(_wi,[_xX,function(_xY){return E(new T(function(){return B(_xy(function(_xZ){var _y0=E(_xZ);return _y0[0]==2?!B(_hS(_y0[1],_oS))?[2]:E(new T(function(){return B(A(_xV,[_xT,function(_y1){return [1,function(_y2){return new F(function(){return A(_wi,[_y2,function(_y3){return E(new T(function(){return B(_xy(function(_y4){var _y5=E(_y4);return _y5[0]==2?!B(_hS(_y5[1],_oQ))?[2]:E(new T(function(){return B(A(_xW,[_y1]));})):[2];}));}));}]);});}];}]));})):[2];}));}));}]);});};},_y6=function(_y7,_y8,_y9){var _ya=function(_yb,_yc){return new F(function(){return _om([1,function(_yd){return new F(function(){return A(_wi,[_yd,function(_ye){return E(new T(function(){return B(_xy(function(_yf){var _yg=E(_yf);if(_yg[0]==4){var _yh=E(_yg[1]);if(!_yh[0]){return new F(function(){return A(_y7,[_yg,_yb,_yc]);});}else{return E(E(_yh[1])[1])==45?E(_yh[2])[0]==0?E([1,function(_yi){return new F(function(){return A(_wi,[_yi,function(_yj){return E(new T(function(){return B(_xy(function(_yk){return new F(function(){return A(_y7,[_yk,_yb,function(_yl){return new F(function(){return A(_yc,[new T(function(){return [0, -E(_yl)[1]];})]);});}]);});}));}));}]);});}]):B(A(_y7,[_yg,_yb,_yc])):B(A(_y7,[_yg,_yb,_yc]));}}else{return new F(function(){return A(_y7,[_yg,_yb,_yc]);});}}));}));}]);});}],new T(function(){return [1,B(_xU(_ya,_yc))];}));});};return new F(function(){return _ya(_y8,_y9);});},_ym=function(_yn,_yo){return [2];},_yp=function(_yq){var _yr=E(_yq);return _yr[0]==0?[1,new T(function(){return B(_qX(new T(function(){return B(_qN(E(_yr[1])[1]));}),_qM,_yr[2]));})]:E(_yr[2])[0]==0?E(_yr[3])[0]==0?[1,new T(function(){return B(_qX(_qL,_qM,_yr[1]));})]:[0]:[0];},_ys=function(_yt){var _yu=E(_yt);if(_yu[0]==5){var _yv=B(_yp(_yu[1]));return _yv[0]==0?E(_ym):function(_yw,_yx){return new F(function(){return A(_yx,[new T(function(){return [0,B(_s2(_yv[1]))];})]);});};}else{return E(_ym);}},_yy=function(_yz,_yA){return new F(function(){return _y6(_ys,_yz,_yA);});},_yB=[0,91],_yC=[1,_yB,_1g],_yD=function(_yE,_yF){var _yG=function(_yH,_yI){return [1,function(_yJ){return new F(function(){return A(_wi,[_yJ,function(_yK){return E(new T(function(){return B(_xy(function(_yL){var _yM=E(_yL);if(_yM[0]==2){var _yN=E(_yM[1]);if(!_yN[0]){return [2];}else{var _yO=_yN[2];switch(E(E(_yN[1])[1])){case 44:return E(_yO)[0]==0?!E(_yH)?[2]:E(new T(function(){return B(A(_yE,[_xT,function(_yP){return new F(function(){return _yG(_gw,function(_yQ){return new F(function(){return A(_yI,[[1,_yP,_yQ]]);});});});}]));})):[2];case 93:return E(_yO)[0]==0?E(new T(function(){return B(A(_yI,[_1g]));})):[2];default:return [2];}}}else{return [2];}}));}));}]);});}];},_yR=function(_yS){return new F(function(){return _om([1,function(_yT){return new F(function(){return A(_wi,[_yT,function(_yU){return E(new T(function(){return B(_xy(function(_yV){var _yW=E(_yV);return _yW[0]==2?!B(_hS(_yW[1],_yC))?[2]:E(new T(function(){return B(_om(B(_yG(_5E,_yS)),new T(function(){return B(A(_yE,[_xT,function(_yX){return new F(function(){return _yG(_gw,function(_yY){return new F(function(){return A(_yS,[[1,_yX,_yY]]);});});});}]));})));})):[2];}));}));}]);});}],new T(function(){return [1,B(_xU(function(_yZ,_z0){return new F(function(){return _yR(_z0);});},_yS))];}));});};return new F(function(){return _yR(_yF);});},_z1=function(_z2,_z3){return new F(function(){return _yD(_yy,_z3);});},_z4=function(_z5){return function(_ay){return new F(function(){return _oc(new T(function(){return B(_y6(_ys,_z5,_pl));}),_ay);});};},_z6=new T(function(){return B(_yD(_yy,_pl));}),_z7=function(_yA){return new F(function(){return _oc(_z6,_yA);});},_z8=[0,_z4,_z7,_yy,_z1],_z9=function(_za){return new F(function(){return _5p(0,E(_za)[1],_1g);});},_zb=function(_zc,_zd){return new F(function(){return _5p(0,E(_zc)[1],_zd);});},_ze=function(_zf,_zg){return new F(function(){return _2Z(_zb,_zf,_zg);});},_zh=function(_zi,_zj,_zk){return new F(function(){return _5p(E(_zi)[1],E(_zj)[1],_zk);});},_zl=[0,_zh,_z9,_ze],_zm=new T(function(){return B(unCStr("GHC.Types"));}),_zn=new T(function(){return B(unCStr("Int"));}),_zo=new T(function(){var _zp=hs_wordToWord64(1521842780),_zq=_zp,_zr=hs_wordToWord64(1346191152),_zs=_zr;return [0,_zq,_zs,[0,_zq,_zs,_jz,_zm,_zn],_1g];}),_zt=function(_zu){return E(_zo);},_zv=function(_zw){return E(E(_zw)[1]);},_zx=function(_zy){return E(E(_zy)[1]);},_zz=function(_zA){return E(E(_zA)[2]);},_zB=function(_zC){return E(E(_zC)[3]);},_zD=function(_zE,_zF){var _zG=new T(function(){return B(_zv(_zE));});return function(_zH){return new F(function(){return A(new T(function(){return B(_zx(_zG));}),[new T(function(){return B(A(_zz,[_zE,_zF]));}),function(_zI){return new F(function(){return A(new T(function(){return B(_zB(_zG));}),[[0,_zI,_zH]]);});}]);});};},_zJ=function(_zK,_zL){return [0,_zK,function(_zM){return new F(function(){return _zD(_zL,_zM);});}];},_zN=function(_zO,_zP,_zQ,_zR){return new F(function(){return A(_zx,[_zO,new T(function(){return B(A(_zP,[_zR]));}),function(_zS){return new F(function(){return A(_zQ,[new T(function(){return E(E(_zS)[1]);}),new T(function(){return E(E(_zS)[2]);})]);});}]);});},_zT=function(_zU,_zV,_zW,_zX){return new F(function(){return A(_zx,[_zU,new T(function(){return B(A(_zV,[_zX]));}),function(_zY){return new F(function(){return A(_zW,[new T(function(){return E(E(_zY)[2]);})]);});}]);});},_zZ=function(_A0,_A1,_A2,_A3){return new F(function(){return _zT(_A0,_A1,_A2,_A3);});},_A4=function(_A5){return E(E(_A5)[4]);},_A6=function(_A7,_A8){return function(_A9){return E(new T(function(){return B(A(_A4,[_A7,_A8]));}));};},_Aa=function(_Ab){return [0,function(_A1,_A2,_A3){return new F(function(){return _zN(_Ab,_A1,_A2,_A3);});},function(_A1,_A2,_A3){return new F(function(){return _zZ(_Ab,_A1,_A2,_A3);});},function(_Ac,_Ad){return new F(function(){return A(new T(function(){return B(_zB(_Ab));}),[[0,_Ac,_Ad]]);});},function(_A3){return new F(function(){return _A6(_Ab,_A3);});}];},_Ae=function(_Af,_Ag,_Ah){return new F(function(){return A(_zB,[_Af,[0,_Ag,_Ah]]);});},_Ai=function(_Aj){return E(E(_Aj)[1]);},_Ak=[0,10],_Al=function(_Am,_An){var _Ao=E(_An);if(!_Ao[0]){return E(_t);}else{var _Ap=_Ao[1],_Aq=E(_Ao[2]);if(!_Aq[0]){var _Ar=E(_Ap);return new F(function(){return _As(_Ak,_Ar[3],_Ar[4]);});}else{return function(_At){return new F(function(){return A(new T(function(){var _Au=E(_Ap);return B(_As(_Ak,_Au[3],_Au[4]));}),[new T(function(){return B(A(_Am,[new T(function(){return B(A(new T(function(){return B(_Al(_Am,_Aq));}),[_At]));})]));})]);});};}}},_Av=new T(function(){return B(unCStr("(->)"));}),_Aw=new T(function(){return B(unCStr("GHC.Prim"));}),_Ax=new T(function(){var _Ay=hs_wordToWord64(4173248105),_Az=_Ay,_AA=hs_wordToWord64(4270398258),_AB=_AA;return [0,_Az,_AB,[0,_Az,_AB,_jz,_Aw,_Av],_1g];}),_AC=new T(function(){return E(E(_Ax)[3]);}),_AD=new T(function(){return B(unCStr("[]"));}),_AE=new T(function(){var _AF=hs_wordToWord64(4033920485),_AG=_AF,_AH=hs_wordToWord64(786266835),_AI=_AH;return [0,_AG,_AI,[0,_AG,_AI,_jz,_zm,_AD],_1g];}),_AJ=[1,_jA,_1g],_AK=function(_AL){var _AM=E(_AL);if(!_AM[0]){return [0];}else{var _AN=E(_AM[1]);return [1,[0,_AN[1],_AN[2]],new T(function(){return B(_AK(_AM[2]));})];}},_AO=new T(function(){var _AP=E(_AE),_AQ=E(_AP[3]),_AR=B(_2J(_AP[4],_AJ));if(!_AR[0]){var _AS=E(_AQ);}else{var _AT=B(_k4(new T(function(){return B(_km(B(_8A(_kA,[1,[0,_AQ[1],_AQ[2]],new T(function(){return B(_AK(_AR));})]))));},1))),_AS=E(_AQ);}var _AU=_AS,_AV=_AU;return _AV;}),_AW=[0,8],_AX=[0,32],_AY=function(_AZ){return [1,_AX,_AZ];},_B0=new T(function(){return B(unCStr(" -> "));}),_B1=[0,9],_B2=[0,93],_B3=[0,91],_B4=[0,41],_B5=[0,44],_B6=function(_AZ){return [1,_B5,_AZ];},_B7=[0,40],_B8=[0,0],_As=function(_B9,_Ba,_Bb){var _Bc=E(_Bb);if(!_Bc[0]){return function(_Bd){return new F(function(){return _2J(E(_Ba)[5],_Bd);});};}else{var _Be=_Bc[1],_Bf=function(_Bg){var _Bh=E(_Ba)[5],_Bi=function(_Bj){var _Bk=new T(function(){return B(_Al(_AY,_Bc));});return E(_B9)[1]<=9?function(_Bl){return new F(function(){return _2J(_Bh,[1,_AX,new T(function(){return B(A(_Bk,[_Bl]));})]);});}:function(_Bm){return [1,_5o,new T(function(){return B(_2J(_Bh,[1,_AX,new T(function(){return B(A(_Bk,[[1,_5n,_Bm]]));})]));})];};},_Bn=E(_Bh);if(!_Bn[0]){return new F(function(){return _Bi(_);});}else{if(E(E(_Bn[1])[1])==40){var _Bo=E(_Bn[2]);if(!_Bo[0]){return new F(function(){return _Bi(_);});}else{if(E(E(_Bo[1])[1])==44){return function(_Bp){return [1,_B7,new T(function(){return B(A(new T(function(){return B(_Al(_B6,_Bc));}),[[1,_B4,_Bp]]));})];};}else{return new F(function(){return _Bi(_);});}}}else{return new F(function(){return _Bi(_);});}}},_Bq=E(_Bc[2]);if(!_Bq[0]){var _Br=E(_Ba),_Bs=E(_AO),_Bt=hs_eqWord64(_Br[1],_Bs[1]),_Bu=_Bt;if(!E(_Bu)){return new F(function(){return _Bf(_);});}else{var _Bv=hs_eqWord64(_Br[2],_Bs[2]),_Bw=_Bv;if(!E(_Bw)){return new F(function(){return _Bf(_);});}else{return function(_Bx){return [1,_B3,new T(function(){return B(A(new T(function(){var _By=E(_Be);return B(_As(_B8,_By[3],_By[4]));}),[[1,_B2,_Bx]]));})];};}}}else{if(!E(_Bq[2])[0]){var _Bz=E(_Ba),_BA=E(_AC),_BB=hs_eqWord64(_Bz[1],_BA[1]),_BC=_BB;if(!E(_BC)){return new F(function(){return _Bf(_);});}else{var _BD=hs_eqWord64(_Bz[2],_BA[2]),_BE=_BD;if(!E(_BE)){return new F(function(){return _Bf(_);});}else{var _BF=new T(function(){var _BG=E(_Bq[1]);return B(_As(_AW,_BG[3],_BG[4]));}),_BH=new T(function(){var _BI=E(_Be);return B(_As(_B1,_BI[3],_BI[4]));});return E(_B9)[1]<=8?function(_BJ){return new F(function(){return A(_BH,[new T(function(){return B(_2J(_B0,new T(function(){return B(A(_BF,[_BJ]));},1)));})]);});}:function(_BK){return [1,_5o,new T(function(){return B(A(_BH,[new T(function(){return B(_2J(_B0,new T(function(){return B(A(_BF,[[1,_5n,_BK]]));},1)));})]));})];};}}}else{return new F(function(){return _Bf(_);});}}}},_BL=function(_BM,_BN){return new F(function(){return A(_BM,[function(_){return new F(function(){return jsFind(toJSStr(E(_BN)));});}]);});},_BO=[0],_BP=function(_BQ){return E(E(_BQ)[3]);},_BR=new T(function(){return [0,"value"];}),_BS=function(_BT){return E(E(_BT)[6]);},_BU=function(_BV){return E(E(_BV)[1]);},_BW=new T(function(){return B(unCStr("Char"));}),_BX=new T(function(){var _BY=hs_wordToWord64(3763641161),_BZ=_BY,_C0=hs_wordToWord64(1343745632),_C1=_C0;return [0,_BZ,_C1,[0,_BZ,_C1,_jz,_zm,_BW],_1g];}),_C2=function(_C3){return E(_BX);},_C4=function(_C5){return E(_AE);},_C6=new T(function(){return B(_kD(_C4,_C2));}),_C7=new T(function(){return B(A(_C6,[_]));}),_C8=function(_C9,_Ca,_Cb,_Cc,_Cd,_Ce,_Cf,_Cg,_Ch){var _Ci=new T(function(){return B(A(_Cc,[_BO]));});return new F(function(){return A(_Ca,[new T(function(){return B(_BL(E(_C9)[2],_Ch));}),function(_Cj){var _Ck=E(_Cj);return _Ck[0]==0?E(_Ci):B(A(_Ca,[new T(function(){return B(A(E(_C9)[2],[function(_){var _Cl=jsGet(E(_Ck[1])[1],E(_BR)[1]),_Cm=_Cl;return [1,new T(function(){return fromJSStr(_Cm);})];}]));}),function(_Cn){var _Co=E(_Cn);if(!_Co[0]){return E(_Ci);}else{var _Cp=_Co[1];if(!E(new T(function(){var _Cq=B(A(_Ce,[_])),_Cr=E(_C7),_Cs=hs_eqWord64(_Cq[1],_Cr[1]),_Ct=_Cs;if(!E(_Ct)){var _Cu=false;}else{var _Cv=hs_eqWord64(_Cq[2],_Cr[2]),_Cw=_Cv,_Cu=E(_Cw)==0?false:true;}var _Cx=_Cu,_Cy=_Cx;return _Cy;}))){var _Cz=function(_CA){return new F(function(){return A(_Cc,[[1,_Cp,new T(function(){return B(A(new T(function(){return B(_BS(_Cg));}),[new T(function(){return B(A(new T(function(){return B(_BP(_Cg));}),[new T(function(){return B(unAppCStr("can\'t read \"",new T(function(){return B(_2J(_Cp,new T(function(){return B(unAppCStr("\" as type ",new T(function(){var _CB=B(A(_Ce,[_]));return B(A(_As,[_B8,_CB[3],_CB[4],_1g]));})));})));})));})]));})]));})]]);});},_CC=B(A(new T(function(){return B(A(_BU,[_Cf,_5D]));}),[_Cp]));if(!_CC[0]){return new F(function(){return _Cz(_);});}else{var _CD=E(_CC[1]);return E(_CD[2])[0]==0?E(_CC[2])[0]==0?B(A(_Cc,[[2,_CD[1]]])):B(_Cz(_)):B(_Cz(_));}}else{return new F(function(){return A(_Cc,[[2,_Cp]]);});}}}]));}]);});},_CE=1,_CF=function(_CG){return E(E(_CG)[9]);},_CH=function(_CI){return E(E(_CI)[2]);},_CJ=function(_CK){return E(E(_CK)[2]);},_CL=function(_CM,_CN,_CO){var _CP=E(_CO);if(!_CP[0]){return [0];}else{var _CQ=_CP[1],_CR=B(A(_CM,[_])),_CS=E(_C7),_CT=hs_eqWord64(_CR[1],_CS[1]),_CU=_CT;if(!E(_CU)){return new F(function(){return A(_CJ,[_CN,_CQ]);});}else{var _CV=hs_eqWord64(_CR[2],_CS[2]),_CW=_CV;return E(_CW)==0?B(A(_CJ,[_CN,_CQ])):E(_CQ);}}},_CX=function(_CY,_CZ,_D0,_D1,_D2){var _D3=new T(function(){return B(_CF(_CY));}),_D4=new T(function(){return B(_zv(_CZ));}),_D5=new T(function(){return B(_zB(_D4));}),_D6=new T(function(){return B(_zB(_D4));}),_D7=new T(function(){return B(_zB(_D4));}),_D8=new T(function(){return B(_zB(_D4));});return function(_D9,_Da,_Db){return function(_Dc){return new F(function(){return A(new T(function(){return B(_zx(_D4));}),[new T(function(){var _Dd=E(_D9);return _Dd[0]==0?B(A(new T(function(){return B(_zx(_D4));}),[new T(function(){return B(A(_D6,[[0,_Dc,_Dc]]));}),function(_De){var _Df=new T(function(){return E(E(_De)[1]);}),_Dg=new T(function(){return E(E(_Df)[2]);});return new F(function(){return A(new T(function(){return B(_zx(_D4));}),[new T(function(){return B(A(_D7,[[0,_c,new T(function(){var _Dh=E(_Df);return [0,_Dh[1],new T(function(){return [0,E(_Dg)[1]+1|0];}),_Dh[3],_Dh[4],_Dh[5],_Dh[6],_Dh[7]];})]]));}),function(_Di){return new F(function(){return A(_D5,[[0,[1,_5v,new T(function(){return B(_2J(B(_5p(0,E(_Dg)[1],_1g)),new T(function(){return E(E(_Df)[1]);},1)));})],new T(function(){return E(E(_Di)[2]);})]]);});}]);});}])):B(A(_D5,[[0,_Dd[1],_Dc]]));}),function(_Dj){var _Dk=new T(function(){return E(E(_Dj)[1]);});return new F(function(){return A(new T(function(){return B(_zx(_D4));}),[new T(function(){var _Dl=new T(function(){return E(E(_Dj)[2]);});return B(A(_D6,[[0,_Dl,_Dl]]));}),function(_Dm){return new F(function(){return A(new T(function(){return B(_zx(_D4));}),[new T(function(){return B(A(_D7,[[0,_c,new T(function(){var _Dn=E(E(_Dm)[1]);return [0,_Dn[1],_Dn[2],_CE,_Dn[4],_Dn[5],_Dn[6],_Dn[7]];})]]));}),function(_Do){return new F(function(){return A(new T(function(){return B(_zx(_D4));}),[new T(function(){return B(A(new T(function(){return B(_C8(new T(function(){return B(_zJ(new T(function(){return B(_Aa(_D4));}),_CZ));}),function(_Dp,_53,_Dq){return new F(function(){return _zN(_D4,_Dp,_53,_Dq);});},function(_Dp,_53,_Dq){return new F(function(){return _zZ(_D4,_Dp,_53,_Dq);});},function(_53,_Dq){return new F(function(){return _Ae(_D4,_53,_Dq);});},function(_Dq){return new F(function(){return _A6(_D4,_Dq);});},_D0,_D2,_CY,_Dk));}),[new T(function(){return E(E(_Do)[2]);})]));}),function(_Dr){var _Ds=E(_Dr),_Dt=_Ds[2],_Du=E(_Ds[1]);switch(_Du[0]){case 0:return new F(function(){return A(_D8,[[0,[0,new T(function(){return B(A(_D3,[_Dk,_Da,new T(function(){return B(_CL(_D0,_D1,_Db));}),_5E,_5h]));}),_5h],_Dt]]);});break;case 1:return new F(function(){return A(_D8,[[0,[0,new T(function(){return B(A(new T(function(){return B(_CH(new T(function(){return B(_Ai(_CY));})));}),[new T(function(){return B(A(_D3,[_Dk,_Da,_Du[1],_5E,_5h]));}),_Du[2]]));}),_5h],_Dt]]);});break;default:var _Dv=_Du[1];return new F(function(){return A(_D8,[[0,[0,new T(function(){return B(A(_D3,[_Dk,_Da,new T(function(){return B(_CL(_D0,_D1,[1,_Dv]));}),_5E,_5h]));}),[1,_Dv]],_Dt]]);});}}]);});}]);});}]);});}]);});};};},_Dw=new T(function(){return B(_CX(_mC,_oa,_zt,_zl,_z8));}),_Dx=function(_Dy,_Dz){return function(_DA,_){var _DB=B(A(_Dy,[_DA,_])),_DC=_DB,_DD=E(_DC),_DE=E(_DD[1]);return [0,[0,function(_DF,_){var _DG=B(A(_DE[1],[_DF,_])),_DH=_DG,_DI=E(_DH),_DJ=jsSetCB(_DI[1],E(new T(function(){return [0,toJSStr(B(_e3(_Dz)))];}))[1],E(new T(function(){return B(_fd(_Dz,function(_){var _DK=E(E(_DA)[4]),_DL=B(A(_DK[1],[_])),_DM=_DL,_DN=E(_DM);if(!_DN[0]){return _c;}else{var _DO=B(A(_DK[2],[_DN[1],_])),_DP=_DO;return _c;}}));}))),_DQ=_DJ;return _DI;},_DE[2]],_DD[2]];};},_DR=function(_DS,_DT,_DU){return function(_DV,_){var _DW=B(_gM(function(_DX,_){var _DY=B(A(new T(function(){return B(_Dx(new T(function(){return B(_j3(new T(function(){return [0,E(_DS)[1]+1|0];}),_jm));}),_dK));}),[_DX,_])),_DZ=_DY,_E0=E(_DZ),_E1=E(_E0[1]),_E2=B(A(new T(function(){return B(_ix(_gr,function(_E3,_){var _E4=B(A(new T(function(){return B(A(_Dw,[_5h,_jk,[1,_DS]]));}),[_E3,_])),_E5=_E4,_E6=E(_E5),_E7=E(_E6[1]);return [0,[0,function(_E8,_){var _E9=B(A(_E7[1],[_E8,_])),_Ea=_E9,_Eb=B(A(_d,[_t,_Ea,_gs,_gu,_])),_Ec=_Eb;return _Ea;},_E7[2]],_E6[2]];},_dL));}),[_E0[2],_])),_Ed=_E2,_Ee=E(_Ed),_Ef=E(_Ee[1]),_Eg=B(A(new T(function(){return B(_Dx(new T(function(){return B(_j3(new T(function(){return [0,E(_DS)[1]-1|0];}),_jo));}),_dK));}),[_Ee[2],_])),_Eh=_Eg,_Ei=E(_Eh),_Ej=E(_Ei[1]);return [0,[0,function(_Ek,_){var _El=B(_bw(_Y,function(_b1,_){return new F(function(){return _aI(_aw,_DT,_b1,_);});},_Ek,_)),_Em=_El,_En=B(A(_d,[_t,_Em,_aA,_c7,_])),_Eo=_En,_Ep=B(_bw(_Y,function(_Eq,_){var _Er=B(A(_E1[1],[_Eq,_])),_Es=_Er,_Et=B(A(_Ef[1],[_Eq,_])),_Eu=_Et,_Ev=B(A(_Ej[1],[_Eq,_])),_Ew=_Ev;return _Eq;},_Ek,_)),_Ex=_Ep,_Ey=B(A(_d,[_t,_Ex,_aA,_c7,_])),_Ez=_Ey;return _Ek;},new T(function(){var _EA=E(_E1[2]);if(!_EA[0]){var _EB=E(_Ef[2]),_EC=_EB[0]==0?E(_Ej[2]):E(_EB);}else{var _EC=E(_EA);}return _EC;})],_Ei[2]];},function(_ED){return function(_EE,_){return [0,new T(function(){var _EF=B(A(_DU,[_ED]));if(!_EF[0]){var _EG=[0];}else{var _EG=[1,function(_b1,_){return new F(function(){return _ba(_aw,new T(function(){return [0,toJSStr(E(_EF[1]))];}),_b1,_);});}];}return _EG;}),_EE];};},_DV,_)),_EH=_DW,_EI=E(_EH),_EJ=E(_EI[1]);return [0,[0,function(_b1,_){return new F(function(){return _bG(_EJ[1],_b1,_);});},_EJ[2]],_EI[2]];};},_EK=new T(function(){return B(unCStr("row vertical-align"));}),_EL=new T(function(){return B(unCStr("br"));}),_EM=function(_EN,_){var _EO=jsCreateElem(toJSStr(E(_EL))),_EP=_EO,_EQ=jsAppendChild(_EP,E(_EN)[1]);return [0,_EP];},_ER=function(_ES){return E(E(_ES)[2]);},_ET=function(_EU){return E(E(_EU)[5]);},_EV=function(_EW){return E(E(_EW)[2]);},_EX=function(_EY){return E(E(_EY)[1]);},_EZ=function(_F0){return E(E(_F0)[3]);},_F1=function(_F2){return E(E(_F2)[4]);},_F3=new T(function(){return B(unCStr("\u0434\u043e\u043b\u0436\u043d\u043e >= 0 \u0438 <= \u043e\u0431\u0449\u0435\u0433\u043e \u0447\u0438\u0441\u043b\u0430 \u0431\u0438\u0442\u043e\u0432"));}),_F4=[1,_F3],_F5=function(_F6){return new F(function(){return _om([1,function(_F7){return new F(function(){return A(_wi,[_F7,function(_F8){return E(new T(function(){return B(_xy(function(_F9){var _Fa=E(_F9);return _Fa[0]==0?B(A(_F6,[_Fa[1]])):[2];}));}));}]);});}],new T(function(){return [1,B(_xU(_Fb,_F6))];}));});},_Fb=function(_Fc,_Fd){return new F(function(){return _F5(_Fd);});},_Fe=function(_Ff){return new F(function(){return _om(B(_om([1,function(_Fg){return new F(function(){return A(_wi,[_Fg,function(_Fh){return E(new T(function(){return B(_xy(function(_Fi){var _Fj=E(_Fi);return _Fj[0]==1?B(A(_Ff,[_Fj[1]])):[2];}));}));}]);});}],new T(function(){return B(_yD(_Fb,_Ff));}))),new T(function(){return [1,B(_xU(_Fk,_Ff))];}));});},_Fk=function(_Fl,_Fm){return new F(function(){return _Fe(_Fm);});},_Fn=new T(function(){return B(_Fe(_pl));}),_Fo=function(_yA){return new F(function(){return _oc(_Fn,_yA);});},_Fp=new T(function(){return B(_F5(_pl));}),_Fq=function(_yA){return new F(function(){return _oc(_Fp,_yA);});},_Fr=function(_Fs){return E(_Fq);},_Ft=[0,_Fr,_Fo,_Fb,_Fk],_Fu=function(_Fv){return E(E(_Fv)[4]);},_Fw=function(_Fx,_Fy,_Fz){return new F(function(){return _yD(new T(function(){return B(_Fu(_Fx));}),_Fz);});},_FA=function(_FB){return function(_ay){return new F(function(){return _oc(new T(function(){return B(_yD(new T(function(){return B(_Fu(_FB));}),_pl));}),_ay);});};},_FC=function(_FD,_FE){return function(_ay){return new F(function(){return _oc(new T(function(){return B(A(_Fu,[_FD,_FE,_pl]));}),_ay);});};},_FF=function(_FG){return [0,function(_yA){return new F(function(){return _FC(_FG,_yA);});},new T(function(){return B(_FA(_FG));}),new T(function(){return B(_Fu(_FG));}),function(_yz,_yA){return new F(function(){return _Fw(_FG,_yz,_yA);});}];},_FH=new T(function(){return B(_FF(_Ft));}),_FI=new T(function(){return B(unCStr("Prelude.(!!): negative index\n"));}),_FJ=new T(function(){return B(err(_FI));}),_FK=new T(function(){return B(unCStr("Prelude.(!!): index too large\n"));}),_FL=new T(function(){return B(err(_FK));}),_FM=function(_FN,_FO){while(1){var _FP=E(_FN);if(!_FP[0]){return E(_FL);}else{var _FQ=E(_FO);if(!_FQ){return E(_FP[1]);}else{_FN=_FP[2];_FO=_FQ-1|0;continue;}}}},_FR=new T(function(){return B(unCStr("ACK"));}),_FS=new T(function(){return B(unCStr("BEL"));}),_FT=new T(function(){return B(unCStr("BS"));}),_FU=new T(function(){return B(unCStr("SP"));}),_FV=[1,_FU,_1g],_FW=new T(function(){return B(unCStr("US"));}),_FX=[1,_FW,_FV],_FY=new T(function(){return B(unCStr("RS"));}),_FZ=[1,_FY,_FX],_G0=new T(function(){return B(unCStr("GS"));}),_G1=[1,_G0,_FZ],_G2=new T(function(){return B(unCStr("FS"));}),_G3=[1,_G2,_G1],_G4=new T(function(){return B(unCStr("ESC"));}),_G5=[1,_G4,_G3],_G6=new T(function(){return B(unCStr("SUB"));}),_G7=[1,_G6,_G5],_G8=new T(function(){return B(unCStr("EM"));}),_G9=[1,_G8,_G7],_Ga=new T(function(){return B(unCStr("CAN"));}),_Gb=[1,_Ga,_G9],_Gc=new T(function(){return B(unCStr("ETB"));}),_Gd=[1,_Gc,_Gb],_Ge=new T(function(){return B(unCStr("SYN"));}),_Gf=[1,_Ge,_Gd],_Gg=new T(function(){return B(unCStr("NAK"));}),_Gh=[1,_Gg,_Gf],_Gi=new T(function(){return B(unCStr("DC4"));}),_Gj=[1,_Gi,_Gh],_Gk=new T(function(){return B(unCStr("DC3"));}),_Gl=[1,_Gk,_Gj],_Gm=new T(function(){return B(unCStr("DC2"));}),_Gn=[1,_Gm,_Gl],_Go=new T(function(){return B(unCStr("DC1"));}),_Gp=[1,_Go,_Gn],_Gq=new T(function(){return B(unCStr("DLE"));}),_Gr=[1,_Gq,_Gp],_Gs=new T(function(){return B(unCStr("SI"));}),_Gt=[1,_Gs,_Gr],_Gu=new T(function(){return B(unCStr("SO"));}),_Gv=[1,_Gu,_Gt],_Gw=new T(function(){return B(unCStr("CR"));}),_Gx=[1,_Gw,_Gv],_Gy=new T(function(){return B(unCStr("FF"));}),_Gz=[1,_Gy,_Gx],_GA=new T(function(){return B(unCStr("VT"));}),_GB=[1,_GA,_Gz],_GC=new T(function(){return B(unCStr("LF"));}),_GD=[1,_GC,_GB],_GE=new T(function(){return B(unCStr("HT"));}),_GF=[1,_GE,_GD],_GG=[1,_FT,_GF],_GH=[1,_FS,_GG],_GI=[1,_FR,_GH],_GJ=new T(function(){return B(unCStr("ENQ"));}),_GK=[1,_GJ,_GI],_GL=new T(function(){return B(unCStr("EOT"));}),_GM=[1,_GL,_GK],_GN=new T(function(){return B(unCStr("ETX"));}),_GO=[1,_GN,_GM],_GP=new T(function(){return B(unCStr("STX"));}),_GQ=[1,_GP,_GO],_GR=new T(function(){return B(unCStr("SOH"));}),_GS=[1,_GR,_GQ],_GT=new T(function(){return B(unCStr("NUL"));}),_GU=[1,_GT,_GS],_GV=[0,92],_GW=new T(function(){return B(unCStr("\\DEL"));}),_GX=new T(function(){return B(unCStr("\\a"));}),_GY=new T(function(){return B(unCStr("\\\\"));}),_GZ=new T(function(){return B(unCStr("\\SO"));}),_H0=new T(function(){return B(unCStr("\\r"));}),_H1=new T(function(){return B(unCStr("\\f"));}),_H2=new T(function(){return B(unCStr("\\v"));}),_H3=new T(function(){return B(unCStr("\\n"));}),_H4=new T(function(){return B(unCStr("\\t"));}),_H5=new T(function(){return B(unCStr("\\b"));}),_H6=function(_H7,_H8){if(_H7<=127){var _H9=E(_H7);switch(_H9){case 92:return new F(function(){return _2J(_GY,_H8);});break;case 127:return new F(function(){return _2J(_GW,_H8);});break;default:if(_H9<32){var _Ha=E(_H9);switch(_Ha){case 7:return new F(function(){return _2J(_GX,_H8);});break;case 8:return new F(function(){return _2J(_H5,_H8);});break;case 9:return new F(function(){return _2J(_H4,_H8);});break;case 10:return new F(function(){return _2J(_H3,_H8);});break;case 11:return new F(function(){return _2J(_H2,_H8);});break;case 12:return new F(function(){return _2J(_H1,_H8);});break;case 13:return new F(function(){return _2J(_H0,_H8);});break;case 14:return new F(function(){return _2J(_GZ,new T(function(){var _Hb=E(_H8);if(!_Hb[0]){var _Hc=[0];}else{var _Hc=E(E(_Hb[1])[1])==72?B(unAppCStr("\\&",_Hb)):E(_Hb);}return _Hc;},1));});break;default:return new F(function(){return _2J([1,_GV,new T(function(){var _Hd=_Ha;return _Hd>=0?B(_FM(_GU,_Hd)):E(_FJ);})],_H8);});}}else{return [1,[0,_H9],_H8];}}}else{return [1,_GV,new T(function(){var _He=jsShowI(_H7),_Hf=_He;return B(_2J(fromJSStr(_Hf),new T(function(){var _Hg=E(_H8);if(!_Hg[0]){var _Hh=[0];}else{var _Hi=E(_Hg[1])[1];if(_Hi<48){var _Hj=E(_Hg);}else{var _Hj=_Hi>57?E(_Hg):B(unAppCStr("\\&",_Hg));}var _Hk=_Hj,_Hl=_Hk,_Hh=_Hl;}return _Hh;},1)));})];}},_Hm=[0,39],_Hn=[1,_Hm,_1g],_Ho=new T(function(){return B(unCStr("\'\\\'\'"));}),_Hp=function(_Hq){var _Hr=E(E(_Hq)[1]);return _Hr==39?E(_Ho):[1,_Hm,new T(function(){return B(_H6(_Hr,_Hn));})];},_Hs=[0,34],_Ht=new T(function(){return B(unCStr("\\\""));}),_Hu=function(_Hv,_Hw){var _Hx=E(_Hv);if(!_Hx[0]){return E(_Hw);}else{var _Hy=_Hx[2],_Hz=E(E(_Hx[1])[1]);if(_Hz==34){return new F(function(){return _2J(_Ht,new T(function(){return B(_Hu(_Hy,_Hw));},1));});}else{return new F(function(){return _H6(_Hz,new T(function(){return B(_Hu(_Hy,_Hw));}));});}}},_HA=function(_HB,_HC){return [1,_Hs,new T(function(){return B(_Hu(_HB,[1,_Hs,_HC]));})];},_HD=function(_HE){return new F(function(){return _2J(_Ho,_HE);});},_HF=function(_HG,_HH){var _HI=E(E(_HH)[1]);return _HI==39?E(_HD):function(_HJ){return [1,_Hm,new T(function(){return B(_H6(_HI,[1,_Hm,_HJ]));})];};},_HK=[0,_HF,_Hp,_HA],_HL=function(_HM){return E(E(_HM)[3]);},_HN=function(_HO,_HP){return new F(function(){return A(_HL,[_HO,_HP,_1g]);});},_HQ=function(_HR,_HS,_HT){return new F(function(){return _2Z(new T(function(){return B(_HL(_HR));}),_HS,_HT);});},_HU=function(_HV){return [0,function(_HW){return E(new T(function(){return B(_HL(_HV));}));},function(_HE){return new F(function(){return _HN(_HV,_HE);});},function(_HX,_HE){return new F(function(){return _HQ(_HV,_HX,_HE);});}];},_HY=new T(function(){return B(_HU(_HK));}),_HZ=new T(function(){return B(_CX(_mC,_oa,_C6,_HY,_FH));}),_I0=new T(function(){return B(unCStr("\u041e\u0431\u043d\u043e\u0432\u0438\u0442\u044c"));}),_I1=[1,_I0],_I2=new T(function(){return B(unCStr("submit"));}),_I3=new T(function(){return B(A(_HZ,[_5h,_I2,_I1]));}),_I4=new T(function(){return B(_Dx(_I3,_dK));}),_I5=new T(function(){return B(unCStr("margin-bottom: 40px"));}),_I6=[0,_aE,_I5],_I7=[1,_I6,_1g],_I8=function(_I9,_Ia,_Ib){var _Ic=function(_Id,_Ie){return new F(function(){return _om([1,function(_If){return new F(function(){return A(_wi,[_If,function(_Ig){return E(new T(function(){return B(_xy(function(_Ih){var _Ii=E(_Ih);if(_Ii[0]==4){var _Ij=E(_Ii[1]);if(!_Ij[0]){return new F(function(){return A(_I9,[_Ii,_Id,_Ie]);});}else{return E(E(_Ij[1])[1])==45?E(_Ij[2])[0]==0?E([1,function(_Ik){return new F(function(){return A(_wi,[_Ik,function(_Il){return E(new T(function(){return B(_xy(function(_Im){return new F(function(){return A(_I9,[_Im,_Id,function(_In){return new F(function(){return A(_Ie,[new T(function(){return [0, -E(_In)[1]];})]);});}]);});}));}));}]);});}]):B(A(_I9,[_Ii,_Id,_Ie])):B(A(_I9,[_Ii,_Id,_Ie]));}}else{return new F(function(){return A(_I9,[_Ii,_Id,_Ie]);});}}));}));}]);});}],new T(function(){return [1,B(_xU(_Ic,_Ie))];}));});};return new F(function(){return _Ic(_Ia,_Ib);});},_Io=new T(function(){return [0,1/0];}),_Ip=function(_Iq,_Ir){return new F(function(){return A(_Ir,[_Io]);});},_Is=new T(function(){return [0,0/0];}),_It=function(_Iu,_Iv){return new F(function(){return A(_Iv,[_Is]);});},_Iw=new T(function(){return B(unCStr("NaN"));}),_Ix=new T(function(){return B(unCStr("Infinity"));}),_Iy=function(_Iz,_IA){return [2];},_IB=[0,1024],_IC=[0,-1021],_ID=new T(function(){return [0,0/0];}),_IE=new T(function(){return [0,-1/0];}),_IF=new T(function(){return [0,1/0];}),_IG=[0,0],_IH=function(_II,_IJ){while(1){var _IK=E(_II);if(!_IK[0]){_II=[1,I_fromInt(_IK[1])];continue;}else{var _IL=E(_IJ);if(!_IL[0]){_II=_IK;_IJ=[1,I_fromInt(_IL[1])];continue;}else{return new F(function(){return I_fromRat(_IK[1],_IL[1]);});}}}},_IM=function(_IN,_IO){return !B(_3j(_IO,_IG))?[0,B(_IH(_IN,_IO))]:!B(_3j(_IN,_IG))?!B(_4d(_IN,_IG))?E(_IF):E(_IE):E(_ID);},_IP=function(_IQ,_IR){while(1){var _IS=E(_IQ);if(!_IS[0]){return E(_IR);}else{_IQ=_IS[2];var _IT=_IR+1|0;_IR=_IT;continue;}}},_IU=function(_IV,_IW){return !B(_3j(_IW,_3r))?B(_3P(_IV,_IW)):E(_3i);},_IX=function(_IY,_IZ){while(1){if(!B(_3j(_IZ,_3r))){var _J0=_IZ,_J1=B(_IU(_IY,_IZ));_IY=_J0;_IZ=_J1;continue;}else{return E(_IY);}}},_J2=function(_J3){var _J4=E(_J3);if(!_J4[0]){var _J5=E(_J4[1]);return _J5==(-2147483648)?E(_qG):_J5<0?[0, -_J5]:E(_J4);}else{var _J6=_J4[1];return I_compareInt(_J6,0)>=0?E(_J4):[1,I_negate(_J6)];}},_J7=5,_J8=new T(function(){return B(_3f(_J7));}),_J9=new T(function(){return die(_J8);}),_Ja=function(_Jb,_Jc){if(!B(_3j(_Jc,_3r))){var _Jd=B(_IX(B(_J2(_Jb)),B(_J2(_Jc))));return !B(_3j(_Jd,_3r))?[0,B(_3H(_Jb,_Jd)),B(_3H(_Jc,_Jd))]:E(_3i);}else{return E(_J9);}},_Je=[0,1],_Jf=new T(function(){return B(unCStr("Negative exponent"));}),_Jg=new T(function(){return B(err(_Jf));}),_Jh=function(_Ji,_Jj,_Jk){while(1){if(!E(_3t)){if(!B(_3j(B(_3P(_Jj,_3s)),_3r))){if(!B(_3j(_Jj,_Je))){var _Jl=B(_qP(_Ji,_Ji)),_Jm=B(_3H(B(_3y(_Jj,_Je)),_3s)),_Jn=B(_qP(_Ji,_Jk));_Ji=_Jl;_Jj=_Jm;_Jk=_Jn;continue;}else{return new F(function(){return _qP(_Ji,_Jk);});}}else{var _Jl=B(_qP(_Ji,_Ji)),_Jm=B(_3H(_Jj,_3s));_Ji=_Jl;_Jj=_Jm;continue;}}else{return E(_3i);}}},_Jo=function(_Jp,_Jq){while(1){if(!E(_3t)){if(!B(_3j(B(_3P(_Jq,_3s)),_3r))){if(!B(_3j(_Jq,_Je))){return new F(function(){return _Jh(B(_qP(_Jp,_Jp)),B(_3H(B(_3y(_Jq,_Je)),_3s)),_Jp);});}else{return E(_Jp);}}else{var _Jr=B(_qP(_Jp,_Jp)),_Js=B(_3H(_Jq,_3s));_Jp=_Jr;_Jq=_Js;continue;}}else{return E(_3i);}}},_Jt=function(_Ju,_Jv){return !B(_4d(_Jv,_3r))?!B(_3j(_Jv,_3r))?B(_Jo(_Ju,_Jv)):E(_Je):E(_Jg);},_Jw=[0,1],_Jx=[0,0],_Jy=[0,-1],_Jz=function(_JA){var _JB=E(_JA);if(!_JB[0]){var _JC=_JB[1];return _JC>=0?E(_JC)==0?E(_Jx):E(_qv):E(_Jy);}else{var _JD=I_compareInt(_JB[1],0);return _JD<=0?E(_JD)==0?E(_Jx):E(_Jy):E(_qv);}},_JE=function(_JF,_JG,_JH){while(1){var _JI=E(_JH);if(!_JI[0]){if(!B(_4d(_JF,_qM))){return [0,B(_qP(_JG,B(_Jt(_qL,_JF)))),_Je];}else{var _JJ=B(_Jt(_qL,B(_qH(_JF))));return new F(function(){return _Ja(B(_qP(_JG,B(_Jz(_JJ)))),B(_J2(_JJ)));});}}else{var _JK=B(_3y(_JF,_Jw)),_JL=B(_qx(B(_qP(_JG,_qL)),B(_qN(E(_JI[1])[1]))));_JH=_JI[2];_JF=_JK;_JG=_JL;continue;}}},_JM=function(_JN,_JO){var _JP=E(_JN);if(!_JP[0]){var _JQ=_JP[1],_JR=E(_JO);return _JR[0]==0?_JQ>=_JR[1]:I_compareInt(_JR[1],_JQ)<=0;}else{var _JS=_JP[1],_JT=E(_JO);return _JT[0]==0?I_compareInt(_JS,_JT[1])>=0:I_compare(_JS,_JT[1])>=0;}},_JU=function(_JV){var _JW=E(_JV);if(!_JW[0]){return new F(function(){return _Ja(B(_qP(B(_qX(new T(function(){return B(_qN(E(_JW[1])[1]));}),_qM,_JW[2])),_Jw)),_Jw);});}else{var _JX=_JW[1],_JY=_JW[3],_JZ=E(_JW[2]);if(!_JZ[0]){var _K0=E(_JY);if(!_K0[0]){return new F(function(){return _Ja(B(_qP(B(_qX(_qL,_qM,_JX)),_Jw)),_Jw);});}else{var _K1=_K0[1];if(!B(_JM(_K1,_qM))){var _K2=B(_Jt(_qL,B(_qH(_K1))));return new F(function(){return _Ja(B(_qP(B(_qX(_qL,_qM,_JX)),B(_Jz(_K2)))),B(_J2(_K2)));});}else{return new F(function(){return _Ja(B(_qP(B(_qP(B(_qX(_qL,_qM,_JX)),B(_Jt(_qL,_K1)))),_Jw)),_Jw);});}}}else{var _K3=_JZ[1],_K4=E(_JY);if(!_K4[0]){return new F(function(){return _JE(_qM,B(_qX(_qL,_qM,_JX)),_K3);});}else{return new F(function(){return _JE(_K4[1],B(_qX(_qL,_qM,_JX)),_K3);});}}}},_K5=function(_K6,_K7){while(1){var _K8=E(_K7);if(!_K8[0]){return [0];}else{if(!B(A(_K6,[_K8[1]]))){return E(_K8);}else{_K7=_K8[2];continue;}}}},_K9=function(_Ka,_Kb){var _Kc=E(_Ka);if(!_Kc[0]){var _Kd=_Kc[1],_Ke=E(_Kb);return _Ke[0]==0?_Kd>_Ke[1]:I_compareInt(_Ke[1],_Kd)<0;}else{var _Kf=_Kc[1],_Kg=E(_Kb);return _Kg[0]==0?I_compareInt(_Kf,_Kg[1])>0:I_compare(_Kf,_Kg[1])>0;}},_Kh=[0,0],_Ki=function(_Kj,_Kk){return E(_Kj)[1]==E(_Kk)[1];},_Kl=function(_Km){return new F(function(){return _Ki(_Kh,_Km);});},_Kn=[0,E(_qM),E(_Je)],_Ko=[1,_Kn],_Kp=[0,-2147483648],_Kq=[0,2147483647],_Kr=function(_Ks,_Kt,_Ku){var _Kv=E(_Ku);if(!_Kv[0]){return [1,new T(function(){var _Kw=B(_JU(_Kv));return [0,E(_Kw[1]),E(_Kw[2])];})];}else{var _Kx=E(_Kv[3]);if(!_Kx[0]){return [1,new T(function(){var _Ky=B(_JU(_Kv));return [0,E(_Ky[1]),E(_Ky[2])];})];}else{var _Kz=_Kx[1];if(!B(_K9(_Kz,_Kq))){if(!B(_4d(_Kz,_Kp))){var _KA=function(_KB){var _KC=_KB+B(_s2(_Kz))|0;return _KC<=(E(_Kt)[1]+3|0)?_KC>=(E(_Ks)[1]-3|0)?[1,new T(function(){var _KD=B(_JU(_Kv));return [0,E(_KD[1]),E(_KD[2])];})]:E(_Ko):[0];},_KE=B(_K5(_Kl,_Kv[1]));if(!_KE[0]){var _KF=E(_Kv[2]);if(!_KF[0]){return E(_Ko);}else{var _KG=B(_eC(_Kl,_KF[1]));if(!E(_KG[2])[0]){return E(_Ko);}else{return new F(function(){return _KA( -B(_IP(_KG[1],0)));});}}}else{return new F(function(){return _KA(B(_IP(_KE,0)));});}}else{return [0];}}else{return [0];}}}},_KH=function(_KI){var _KJ=E(_KI);switch(_KJ[0]){case 3:var _KK=_KJ[1];return !B(_hS(_KK,_Ix))?!B(_hS(_KK,_Iw))?E(_Iy):E(_It):E(_Ip);case 5:var _KL=B(_Kr(_IC,_IB,_KJ[1]));return _KL[0]==0?E(_Ip):function(_KM,_KN){return new F(function(){return A(_KN,[new T(function(){var _KO=E(_KL[1]);return B(_IM(_KO[1],_KO[2]));})]);});};default:return E(_Iy);}},_KP=function(_yz,_yA){return new F(function(){return _I8(_KH,_yz,_yA);});},_KQ=function(_KR,_KS){return new F(function(){return _yD(_KP,_KS);});},_KT=function(_KU){return function(_ay){return new F(function(){return _oc(new T(function(){return B(_I8(_KH,_KU,_pl));}),_ay);});};},_KV=new T(function(){return B(_yD(_KP,_pl));}),_KW=function(_yA){return new F(function(){return _oc(_KV,_yA);});},_KX=[0,_KT,_KW,_KP,_KQ],_KY=function(_KZ){var _L0=jsShow(E(_KZ)[1]),_L1=_L0;return new F(function(){return fromJSStr(_L1);});},_L2=function(_L3){return function(_ay){return new F(function(){return _2J(new T(function(){return B(_KY(_L3));}),_ay);});};},_L4=[0,45],_L5=function(_L6,_L7,_L8){var _L9=function(_La){var _Lb=new T(function(){return B(A(_L6,[[0, -_L8]]));});return E(_L7)[1]<=6?function(_Lc){return [1,_L4,new T(function(){return B(A(_Lb,[_Lc]));})];}:function(_Ld){return [1,_5o,[1,_L4,new T(function(){return B(A(_Lb,[[1,_5n,_Ld]]));})]];};};if(_L8>=0){var _Le=isDoubleNegativeZero(_L8),_Lf=_Le;return E(_Lf)==0?B(A(_L6,[[0,_L8]])):B(_L9(_));}else{return new F(function(){return _L9(_);});}},_Lg=function(_Lh){return new F(function(){return A(_L5,[_L2,_B8,E(_Lh)[1],_1g]);});},_Li=[0,0],_Lj=function(_Lk){return new F(function(){return _L5(_L2,_Li,E(_Lk)[1]);});},_Ll=function(_Lm,_Ln){return new F(function(){return _2Z(_Lj,_Lm,_Ln);});},_Lo=function(_Lp,_Lq){return new F(function(){return _L5(_L2,_Lp,E(_Lq)[1]);});},_Lr=[0,_Lo,_Lg,_Ll],_Ls=new T(function(){return B(unCStr("Double"));}),_Lt=new T(function(){var _Lu=hs_wordToWord64(2568654869),_Lv=_Lu,_Lw=hs_wordToWord64(3333976055),_Lx=_Lw;return [0,_Lv,_Lx,[0,_Lv,_Lx,_jz,_zm,_Ls],_1g];}),_Ly=function(_Lz){return E(_Lt);},_LA=new T(function(){return B(_CX(_mC,_oa,_Ly,_Lr,_KX));}),_LB=function(_LC){return E(E(_LC)[10]);},_LD=function(_LE,_LF){var _LG=new T(function(){return B(_LB(_LE));}),_LH=new T(function(){return B(_zv(_LF));}),_LI=new T(function(){return B(_zB(_LH));});return function(_LJ,_LK){return new F(function(){return A(new T(function(){return B(_zx(_LH));}),[new T(function(){return B(A(new T(function(){return B(_zx(_LH));}),[new T(function(){return B(A(new T(function(){return B(_zB(_LH));}),[[0,_LK,_LK]]));}),function(_LL){var _LM=new T(function(){return E(E(_LL)[1]);}),_LN=new T(function(){return E(E(_LM)[2]);});return new F(function(){return A(new T(function(){return B(_zx(_LH));}),[new T(function(){return B(A(new T(function(){return B(_zB(_LH));}),[[0,_c,new T(function(){var _LO=E(_LM);return [0,_LO[1],new T(function(){return [0,E(_LN)[1]+1|0];}),_LO[3],_LO[4],_LO[5],_LO[6],_LO[7]];})]]));}),function(_LP){return new F(function(){return A(new T(function(){return B(_zB(_LH));}),[[0,[1,_5v,new T(function(){return B(_2J(B(_5p(0,E(_LN)[1],_1g)),new T(function(){return E(E(_LM)[1]);},1)));})],new T(function(){return E(E(_LP)[2]);})]]);});}]);});}]));}),function(_LQ){var _LR=new T(function(){return E(E(_LQ)[1]);});return new F(function(){return A(new T(function(){return B(_zx(_LH));}),[new T(function(){return B(A(_C8,[new T(function(){return B(_zJ(new T(function(){return B(_Aa(_LH));}),_LF));}),function(_Dp,_53,_Dq){return new F(function(){return _zN(_LH,_Dp,_53,_Dq);});},function(_Dp,_53,_Dq){return new F(function(){return _zZ(_LH,_Dp,_53,_Dq);});},function(_53,_Dq){return new F(function(){return _Ae(_LH,_53,_Dq);});},function(_Dq){return new F(function(){return _A6(_LH,_Dq);});},_C6,_FH,_LE,_LR,new T(function(){return E(E(_LQ)[2]);})]));}),function(_LS){var _LT=E(_LS),_LU=_LT[2],_LV=E(_LT[1]);switch(_LV[0]){case 0:return new F(function(){return A(_LI,[[0,[0,new T(function(){return B(A(_LG,[_LR,_LJ]));}),_5h],_LU]]);});break;case 1:return new F(function(){return A(_LI,[[0,[0,new T(function(){return B(A(_LG,[_LR,_LV[1]]));}),_5h],_LU]]);});break;default:var _LW=_LV[1];return new F(function(){return A(_LI,[[0,[0,new T(function(){return B(A(_LG,[_LR,_LW]));}),[1,_LW]],_LU]]);});}}]);});}]);});};},_LX=new T(function(){return B(_LD(_mC,_oa));}),_LY=new T(function(){return [0,"\u0427\u0438\u0441\u043b\u043e \u0431\u0438\u0442\u043e\u0432: "];}),_LZ=new T(function(){return B(unCStr("\u0447\u0438\u0441\u043b\u043e \u0431\u0438\u0442\u043e\u0432 \u0434\u043e\u043b\u0436\u043d\u043e \u0431\u044b\u0442\u044c \u043f\u043e\u043b\u043e\u0436\u0438\u0442\u0435\u043b\u044c\u043d\u043e"));}),_M0=[1,_LZ],_M1=function(_M2){return E(_M2)[1]<=0?E(_M0):[0];},_M3=new T(function(){return [0,"\u0427\u0438\u0441\u043b\u043e \u0431\u0438\u0442\u043e\u0432 \u0434\u043e \u0437\u0430\u043f\u044f\u0442\u043e\u0439: "];}),_M4=function(_M5){return E(E(_M5)[3]);},_M6=function(_M7){return E(E(_M7)[1]);},_M8=function(_M9){return E(E(_M9)[4]);},_Ma=function(_Mb){return E(E(_Mb)[6]);},_Mc=function(_Md){var _Me=new T(function(){return E(E(_Md)[5]);});return function(_Mf,_){var _Mg=B(_6J(new T(function(){return B(_DR(new T(function(){return B(_EV(_Md));}),_LY,_M1));}),function(_Mh,_Mi,_){return [0,[0,_K,[1,new T(function(){var _Mj=E(_Md);return [0,_Mj[1],_Mh,_Mj[3],_Mj[4],_Mj[5]];})]],_Mi];},_Mf,_)),_Mk=_Mg,_Ml=E(_Mk),_Mm=E(_Ml[1]),_Mn=B(_6J(new T(function(){return B(_DR(new T(function(){return B(_EX(_Md));}),_M3,function(_Mo){var _Mp=E(_Mo)[1];return _Mp<=0?E(_F4):_Mp>E(E(_Md)[2])[1]?E(_F4):[0];}));}),function(_Mq,_Mr,_){return [0,[0,_K,[1,new T(function(){var _Ms=E(_Md);return [0,_Mq,_Ms[2],_Ms[3],_Ms[4],_Ms[5]];})]],_Mr];},_Ml[2],_)),_Mt=_Mn,_Mu=E(_Mt),_Mv=E(_Mu[1]),_Mw=B(_6J(function(_Mx,_){var _My=B(A(new T(function(){return B(A(_LA,[_5h,_jk,[1,new T(function(){return B(_EZ(_Md));})]]));}),[_Mx,_])),_Mz=_My,_MA=E(_Mz),_MB=E(_MA[1]),_MC=B(A(_I4,[_MA[2],_])),_MD=_MC,_ME=E(_MD);return [0,[0,function(_MF,_){var _MG=B(_bw(_Y,function(_MH,_){var _MI=B(_bw(_Y,_b0,_MH,_)),_MJ=_MI,_MK=B(A(_d,[_t,_MJ,_aA,_c7,_])),_ML=_MK,_MM=B(_bw(_Y,_MB[1],_MH,_)),_MN=_MM,_MO=B(A(_d,[_t,_MN,_aA,_c7,_])),_MP=_MO;return _MH;},_MF,_)),_MQ=_MG,_MR=B(A(_d,[_t,_MQ,_aA,_b2,_])),_MS=_MR,_MT=B(A(_d,[_t,_MQ,_aE,_aY,_])),_MU=_MT,_MV=B(_bw(_Y,function(_MW,_){var _MX=B(_bw(_Y,E(_ME[1])[1],_MW,_)),_MY=_MX,_MZ=B(A(_d,[_t,_MY,_aA,_c7,_])),_N0=_MZ;return _MY;},_MF,_)),_N1=_MV,_N2=B(A(_d,[_t,_N1,_aA,_b2,_])),_N3=_N2;return _MF;},_MB[2]],_ME[2]];},function(_N4,_N5,_){return [0,[0,_K,[1,new T(function(){var _N6=E(_Md);return [0,_N6[1],_N6[2],_N4,_N6[4],_N6[5]];})]],_N5];},_Mu[2],_)),_N7=_Mw,_N8=E(_N7),_N9=E(_N8[1]),_Na=B(_6J(function(_Nb,_){var _Nc=B(_gM(new T(function(){return B(A(_LA,[_5h,_jk,[1,new T(function(){return B(_M6(_Me));})]]));}),_bm,_Nb,_)),_Nd=_Nc,_Ne=E(_Nd),_Nf=E(_Ne[1]),_Ng=B(_gM(new T(function(){return B(A(_LA,[_5h,_jk,[1,new T(function(){return B(_ER(_Me));})]]));}),_bQ,_Ne[2],_)),_Nh=_Ng,_Ni=E(_Nh),_Nj=E(_Ni[1]),_Nk=B(_gM(new T(function(){return B(A(_Dw,[_5h,_jk,[1,new T(function(){return B(_M4(_Me));})]]));}),_c2,_Ni[2],_)),_Nl=_Nk,_Nm=E(_Nl),_Nn=E(_Nm[1]),_No=B(_gM(new T(function(){return B(A(_Dw,[_5h,_jk,[1,new T(function(){return B(_M8(_Me));})]]));}),_c2,_Nm[2],_)),_Np=_No,_Nq=E(_Np),_Nr=E(_Nq[1]),_Ns=B(_gM(new T(function(){return B(A(_Dw,[_5h,_jk,[1,new T(function(){return B(_ET(_Me));})]]));}),_c2,_Nq[2],_)),_Nt=_Ns,_Nu=E(_Nt),_Nv=E(_Nu[1]),_Nw=B(_6J(function(_Nx,_){var _Ny=B(A(new T(function(){return B(A(_LA,[_5h,_jk,new T(function(){return B(_Ma(_Me));})]));}),[_Nx,_])),_Nz=_Ny,_NA=E(_Nz),_NB=E(_NA[1]);return [0,[0,function(_b1,_){return new F(function(){return _bG(function(_NC,_){var _ND=B(_bw(_Y,_ce,_NC,_)),_NE=_ND,_NF=B(A(_d,[_t,_NE,_aA,_c7,_])),_NG=_NF,_NH=B(_bw(_Y,_NB[1],_NC,_)),_NI=_NH,_NJ=B(A(_d,[_t,_NI,_aA,_c7,_])),_NK=_NJ;return _NC;},_b1,_);});},_NB[2]],_NA[2]];},_ca,_Nu[2],_)),_NL=_Nw,_NM=E(_NL),_NN=E(_NM[1]),_NO=B(A(_I4,[_NM[2],_])),_NP=_NO,_NQ=E(_NP);return [0,[0,function(_b1,_){return new F(function(){return _bG(function(_NR,_){var _NS=B(_aI(_aw,_b3,_NR,_)),_NT=_NS,_NU=B(A(_d,[_t,_NT,_aE,_b4,_])),_NV=_NU,_NW=B(_bw(_Y,function(_NX,_){var _NY=B(_bw(_Y,_b6,_NX,_)),_NZ=_NY,_O0=B(A(_d,[_t,_NZ,_aA,_c7,_])),_O1=_O0,_O2=B(_bw(_Y,_Nf[1],_NX,_)),_O3=_O2,_O4=B(A(_d,[_t,_O3,_aA,_c7,_])),_O5=_O4;return _NX;},_NR,_)),_O6=_NW,_O7=B(A(_d,[_t,_O6,_aA,_b2,_])),_O8=_O7,_O9=B(_bw(_Y,function(_Oa,_){var _Ob=B(_bw(_Y,_bu,_Oa,_)),_Oc=_Ob,_Od=B(A(_d,[_t,_Oc,_aA,_c7,_])),_Oe=_Od,_Of=B(_bw(_Y,_Nj[1],_Oa,_)),_Og=_Of,_Oh=B(A(_d,[_t,_Og,_aA,_c7,_])),_Oi=_Oh;return _Oa;},_NR,_)),_Oj=_O9,_Ok=B(A(_d,[_t,_Oj,_aA,_b2,_])),_Ol=_Ok,_Om=B(_bw(_Y,function(_On,_){var _Oo=B(_bw(_Y,_bY,_On,_)),_Op=_Oo,_Oq=B(A(_d,[_t,_Op,_aA,_c7,_])),_Or=_Oq,_Os=B(_bw(_Y,_Nn[1],_On,_)),_Ot=_Os,_Ou=B(A(_d,[_t,_Ot,_aA,_c7,_])),_Ov=_Ou;return _On;},_NR,_)),_Ow=_Om,_Ox=B(A(_d,[_t,_Ow,_aA,_b2,_])),_Oy=_Ox,_Oz=B(_bw(_Y,function(_OA,_){var _OB=B(_bw(_Y,_c6,_OA,_)),_OC=_OB,_OD=B(A(_d,[_t,_OC,_aA,_c7,_])),_OE=_OD,_OF=B(_bw(_Y,_Nr[1],_OA,_)),_OG=_OF,_OH=B(A(_d,[_t,_OG,_aA,_c7,_])),_OI=_OH;return _OA;},_NR,_)),_OJ=_Oz,_OK=B(A(_d,[_t,_OJ,_aA,_b2,_])),_OL=_OK,_OM=B(_bw(_Y,function(_ON,_){var _OO=B(_bw(_Y,_c9,_ON,_)),_OP=_OO,_OQ=B(A(_d,[_t,_OP,_aA,_c7,_])),_OR=_OQ,_OS=B(_bw(_Y,_Nv[1],_ON,_)),_OT=_OS,_OU=B(A(_d,[_t,_OT,_aA,_c7,_])),_OV=_OU;return _ON;},_NR,_)),_OW=_OM,_OX=B(A(_d,[_t,_OW,_aA,_b2,_])),_OY=_OX,_OZ=B(A(_NN[1],[_NR,_])),_P0=_OZ,_P1=B(A(E(_NQ[1])[1],[_NR,_])),_P2=_P1;return _NR;},_b1,_);});},new T(function(){var _P3=E(_Nf[2]);if(!_P3[0]){var _P4=[0];}else{var _P5=E(_Nj[2]);if(!_P5[0]){var _P6=[0];}else{var _P7=E(_Nn[2]);if(!_P7[0]){var _P8=[0];}else{var _P9=E(_Nr[2]);if(!_P9[0]){var _Pa=[0];}else{var _Pb=E(_Nv[2]);if(!_Pb[0]){var _Pc=[0];}else{var _Pd=E(_NN[2]),_Pc=_Pd[0]==0?[0]:[1,[0,_P3[1],_P5[1],_P7[1],_P9[1],_Pb[1],_Pd[1]]];}var _Pa=_Pc;}var _P8=_Pa;}var _P6=_P8;}var _P4=_P6;}return _P4;})],_NQ[2]];},function(_Pe,_Pf,_){return [0,[0,_K,[1,new T(function(){var _Pg=E(_Md);return [0,_Pg[1],_Pg[2],_Pg[3],_Pg[4],_Pe];})]],_Pf];},_N8[2],_)),_Ph=_Na,_Pi=E(_Ph),_Pj=E(_Pi[1]),_Pk=B(_6J(function(_Pl,_){var _Pm=B(A(new T(function(){return B(A(_LX,[new T(function(){return B(_F1(_Md));})]));}),[_Pl,_])),_Pn=_Pm,_Po=E(_Pn),_Pp=E(_Po[1]),_Pq=B(A(_I4,[_Po[2],_])),_Pr=_Pq,_Ps=E(_Pr);return [0,[0,function(_b1,_){return new F(function(){return _bG(function(_Pt,_){var _Pu=B(_bw(_Y,_cg,_Pt,_)),_Pv=_Pu,_Pw=B(A(_d,[_t,_Pv,_aA,_b2,_])),_Px=_Pw,_Py=B(_bw(_Y,function(_Pz,_){var _PA=B(A(_Pp[1],[_Pz,_])),_PB=_PA,_PC=B(A(_d,[_t,_PB,_cm,_co,_])),_PD=_PC,_PE=B(A(_d,[_t,_PB,_cp,_cq,_])),_PF=_PE,_PG=B(_EM(_Pz,_)),_PH=_PG;return _Pz;},_Pt,_)),_PI=_Py,_PJ=B(A(_d,[_t,_PI,_aA,_b2,_])),_PK=_PJ,_PL=B(A(new T(function(){return B(_4W(E(_Ps[1])[1],_I7));}),[_Pt,_])),_PM=_PL,_PN=B(_bw(_Y,_dJ,_Pt,_)),_PO=_PN,_PP=B(A(_d,[_t,_PO,_aA,_b2,_])),_PQ=_PP;return _Pt;},_b1,_);});},_Pp[2]],_Ps[2]];},function(_PR,_PS,_){return [0,[0,_K,[1,new T(function(){var _PT=E(_Md);return [0,_PT[1],_PT[2],_PT[3],_PR,_PT[5]];})]],_PS];},_Pi[2],_)),_PU=_Pk,_PV=E(_PU),_PW=E(_PV[1]);return [0,[0,function(_PX,_){var _PY=B(_bw(_Y,function(_PZ,_){var _Q0=B(_bw(_Y,_K,_PZ,_)),_Q1=_Q0,_Q2=B(A(_d,[_t,_Q1,_aA,_aB,_])),_Q3=_Q2,_Q4=B(_bw(_Y,function(_Q5,_){var _Q6=B(_bw(_Y,function(_Q7,_){var _Q8=B(_bw(_Y,_aS,_Q7,_)),_Q9=_Q8,_Qa=B(A(_d,[_t,_Q9,_aA,_b2,_])),_Qb=_Qa,_Qc=B(_bw(_Y,function(_Qd,_){var _Qe=B(A(_Mm[1],[_Qd,_])),_Qf=_Qe,_Qg=B(A(_Mv[1],[_Qd,_])),_Qh=_Qg,_Qi=B(A(_N9[1],[_Qd,_])),_Qj=_Qi;return _Qd;},_Q7,_)),_Qk=_Qc,_Ql=B(A(_d,[_t,_Qk,_aA,_b2,_])),_Qm=_Ql,_Qn=B(A(_Pj[1],[_Q7,_])),_Qo=_Qn,_Qp=B(A(_PW[1],[_Q7,_])),_Qq=_Qp;return _Q7;},_Q5,_)),_Qr=_Q6,_Qs=B(A(_d,[_t,_Qr,_aA,_aD,_])),_Qt=_Qs;return _Qr;},_PZ,_)),_Qu=_Q4,_Qv=B(A(_d,[_t,_Qu,_aA,_aC,_])),_Qw=_Qv;return _PZ;},_PX,_)),_Qx=_PY,_Qy=B(A(_d,[_t,_Qx,_aA,_EK,_])),_Qz=_Qy;return _Qx;},new T(function(){var _QA=E(_Mm[2]);if(!_QA[0]){var _QB=E(_Mv[2]);if(!_QB[0]){var _QC=E(_N9[2]);if(!_QC[0]){var _QD=E(_Pj[2]),_QE=_QD[0]==0?E(_PW[2]):E(_QD);}else{var _QE=E(_QC);}var _QF=_QE;}else{var _QF=E(_QB);}var _QG=_QF;}else{var _QG=E(_QA);}return _QG;})],_PV[2]];};},_QH=new T(function(){return B(unCStr("Chromosomes have different lengths"));}),_QI=new T(function(){return B(err(_QH));}),_QJ=function(_QK){return E(_QI);},_QL=new T(function(){return B(unCStr("Irrefutable pattern failed for pattern"));}),_QM=function(_QN){return new F(function(){return _ez([0,new T(function(){return B(_eO(_QN,_QL));})],_ew);});},_QO=function(_QP){return new F(function(){return _QM("Binary/Genetic.hs:38:11-22|[b0, b1]");});},_QQ=new T(function(){return B(_QO(_));}),_QR=function(_QS){return new F(function(){return _QM("Binary/Genetic.hs:37:11-22|[a0, a1]");});},_QT=new T(function(){return B(_QR(_));}),_QU=function(_QV,_QW,_){var _QX=B(A(_QW,[_])),_QY=_QX;return new T(function(){return B(A(_QV,[_QY]));});},_QZ=function(_R0,_R1){return [0,E(_R1)[1],_R0];},_R2=function(_R3,_R4){var _R5=E(_R4);return [0,_R5[1],new T(function(){return B(A(_R3,[_R5[2]]));})];},_R6=[0,_R2,_QZ],_R7=function(_R8,_){return [1,[0,_1g,_R8]];},_R9=function(_Ra,_Rb){return [0,imul(E(_Ra)[1],E(_Rb)[1])|0];},_Rc=function(_Rd,_Re){return [0,E(_Rd)[1]+E(_Re)[1]|0];},_Rf=function(_Rg,_Rh){return [0,E(_Rg)[1]-E(_Rh)[1]|0];},_Ri=function(_Rj){var _Rk=E(_Rj),_Rl=_Rk[1];return _Rl<0?[0, -_Rl]:E(_Rk);},_Rm=function(_Rn){return [0,B(_s2(_Rn))];},_Ro=function(_Rp){return [0, -E(_Rp)[1]];},_Rq=[0,-1],_Rr=[0,0],_Rs=[0,1],_Rt=function(_Ru){var _Rv=E(_Ru)[1];return _Rv>=0?E(_Rv)==0?E(_Rr):E(_Rs):E(_Rq);},_Rw=[0,_Rc,_R9,_Rf,_Ro,_Ri,_Rt,_Rm],_Rx=[0,2147483646],_Ry=[0,0],_Rz=[0,_Ry,_Rx],_RA=function(_RB){return E(_Rz);},_RC=function(_RD){var _RE=jsTrunc(_RD),_RF=_RE;return [0,_RF];},_RG=new T(function(){return B(_a("(function(s){return s[0];})"));}),_RH=function(_RI,_){var _RJ=B(A(_RG,[E(_RI),_])),_RK=_RJ;return new T(function(){return B(_RC(_RK));});},_RL=function(_RM,_){return new F(function(){return _RH(_RM,_);});},_RN=function(_RO,_RP){var _RQ=_RO%_RP;if(_RO<=0){if(_RO>=0){return E(_RQ);}else{if(_RP<=0){return E(_RQ);}else{var _RR=E(_RQ);return _RR==0?0:_RR+_RP|0;}}}else{if(_RP>=0){if(_RO>=0){return E(_RQ);}else{if(_RP<=0){return E(_RQ);}else{var _RS=E(_RQ);return _RS==0?0:_RS+_RP|0;}}}else{var _RT=E(_RQ);return _RT==0?0:_RT+_RP|0;}}},_RU=new T(function(){return B(_a("(function(s){return md51(s.join(\',\'));})"));}),_RV=function(_RW,_){return new F(function(){return A(_RU,[E(_RW),_]);});},_RX=function(_RM,_){return new F(function(){return _RV(_RM,_);});},_RY=function(_RZ){return new F(function(){return _6(function(_){var _=0;return new F(function(){return _RX(_RZ,_);});});});},_S0=function(_S1,_S2,_S3){while(1){var _S4=(function(_S5,_S6,_S7){if(_S5>_S6){var _S8=_S6,_S9=_S5,_Sa=_S7;_S1=_S8;_S2=_S9;_S3=_Sa;return null;}else{return [0,new T(function(){var _Sb=(_S6-_S5|0)+1|0;switch(_Sb){case -1:var _Sc=[0,_S5];break;case 0:var _Sc=E(_3i);break;default:var _Sc=[0,B(_RN(B(_6(function(_){var _=0;return new F(function(){return _RL(_S7,_);});}))[1],_Sb))+_S5|0];}var _Sd=_Sc;return _Sd;}),new T(function(){return B(_RY(_S7));})];}})(_S1,_S2,_S3);if(_S4!=null){return _S4;}}},_Se=function(_Sf){var _Sg=new T(function(){var _Sh=B(_S0(0,2147483646,_Sf));return [0,_Sh[1],_Sh[2]];});return [0,new T(function(){return E(E(_Sg)[1]);}),new T(function(){return E(E(_Sg)[2]);})];},_Si=function(_Sj){return [0,_Sj,new T(function(){return B(_RY(_Sj));})];},_Sk=[0,_Se,_RA,_Si],_Sl=function(_Sm){return E(E(_Sm)[1]);},_Sn=function(_So,_Sp,_Sq,_Sr,_Ss){return new F(function(){return A(_Sp,[_Sr,function(_St){var _Su=E(_St);return _Su[0]==0?B(A(_Sq,[[0,new T(function(){return B(A(new T(function(){return B(_Sl(_So));}),[function(_Sv){return new F(function(){return _Sn(_So,_Sp,_Sq,_Sv,_Ss);});},_Su[1]]));})]])):B(A(_Ss,[_Su[1]]));}]);});},_Sw=function(_Sx,_Sy,_Sz,_SA){return new F(function(){return A(_Sy,[function(_SB){var _SC=E(_SB);return _SC[0]==0?[0,new T(function(){return B(A(new T(function(){return B(_Sl(_Sx));}),[function(_SD){return new F(function(){return _Sw(_Sx,_Sy,_Sz,_SD);});},_SC[1]]));})]:[1,new T(function(){return B(A(_Sz,[_SC[1]]));})];},_SA]);});},_SE=[0,1],_SF=function(_SG){return E(E(_SG)[7]);},_SH=function(_SI){return E(E(_SI)[2]);},_SJ=[0,0],_SK=[0,1000],_SL=function(_SM,_SN){while(1){var _SO=E(_SM);if(!_SO[0]){var _SP=E(_SO[1]);if(_SP==(-2147483648)){_SM=[1,I_fromInt(-2147483648)];continue;}else{var _SQ=E(_SN);if(!_SQ[0]){return [0,B(_RN(_SP,_SQ[1]))];}else{_SM=[1,I_fromInt(_SP)];_SN=_SQ;continue;}}}else{var _SR=_SO[1],_SS=E(_SN);return _SS[0]==0?[0,I_toInt(I_mod(_SR,I_fromInt(_SS[1])))]:[1,I_mod(_SR,_SS[1])];}}},_ST=function(_SU){return E(E(_SU)[1]);},_SV=function(_SW,_SX,_SY,_SZ,_T0){while(1){var _T1=(function(_T2,_T3,_T4,_T5,_T6){if(!B(_K9(_T4,_T5))){var _T7=B(_qx(B(_3y(_T5,_T4)),_SE)),_T8=new T(function(){return B(A(_SH,[_T2,_T6]));}),_T9=new T(function(){return E(E(_T8)[1]);}),_Ta=new T(function(){return B(_qx(B(_3y(B(_qN(E(E(_T8)[2])[1])),B(_qN(E(_T9)[1])))),_SE));}),_Tb=B((function(_Tc,_Td,_Te){while(1){if(!B(_JM(_Tc,B(_qP(_T7,_SK))))){var _Tf=B(A(new T(function(){return B(_ST(_T2));}),[_Te])),_Tg=B(_qP(_Tc,_Ta)),_Th=B(_qx(B(_qP(_Td,_Ta)),B(_3y(B(_qN(E(_Tf[1])[1])),new T(function(){return B(_qN(E(_T9)[1]));})))));_Te=_Tf[2];_Tc=_Tg;_Td=_Th;continue;}else{return [0,_Td,_Te];}}})(_SE,_SJ,_T6));return [0,new T(function(){return B(A(_SF,[_T3,new T(function(){if(!B(_3j(_T7,_SJ))){var _Ti=B(_qx(_T4,B(_SL(_Tb[1],_T7))));}else{var _Ti=E(_3i);}return _Ti;})]));}),_Tb[2]];}else{var _Tj=_T2,_Tk=_T3,_Tl=_T5,_Tm=_T4,_Tn=_T6;_SW=_Tj;_SX=_Tk;_SY=_Tl;_SZ=_Tm;_T0=_Tn;return null;}})(_SW,_SX,_SY,_SZ,_T0);if(_T1!=null){return _T1;}}},_To=function(_Tp,_Tq){while(1){var _Tr=E(_Tp);if(!_Tr){return E(_Tq);}else{var _Ts=E(_Tq);if(!_Ts[0]){return [0];}else{_Tp=_Tr-1|0;_Tq=_Ts[2];continue;}}}},_Tt=function(_Tu,_Tv){var _Tw=E(_Tu);if(!_Tw){return [0];}else{var _Tx=E(_Tv);return _Tx[0]==0?[0]:[1,_Tx[1],new T(function(){return B(_Tt(_Tw-1|0,_Tx[2]));})];}},_Ty=function(_Tz,_TA){return _Tz<0?[0]:B(_Tt(_Tz,_TA));},_TB=function(_TC,_TD,_TE){var _TF=_TD-_TC|0;if(_TF>0){if(_TC>=0){return _TF<0?[0]:B(_Tt(_TF,new T(function(){return B(_To(_TC,_TE));},1)));}else{return new F(function(){return _Ty(_TF,_TE);});}}else{return [0];}},_TG=function(_TH,_TI,_TJ){return new F(function(){return _Sn(_R6,_nZ,_K,new T(function(){return B(A(_TH,[_TJ]));}),function(_TK){return new F(function(){return _Sn(_R6,_nZ,_K,new T(function(){return B(A(_TI,[new T(function(){return E(E(_TK)[2]);})]));}),function(_TL,_){return [1,[0,[1,new T(function(){return E(E(_TK)[1]);}),new T(function(){return E(E(_TL)[1]);})],new T(function(){return E(E(_TL)[2]);})]];});});});});},_TM=function(_TN,_TO){return _TN>=_TO?_TN!=_TO?2:1:0;},_TP=function(_TQ,_TR){return new F(function(){return _TM(E(_TQ)[1],E(_TR)[1]);});},_TS=[1,_1g,_1g],_TT=function(_TU,_TV){var _TW=function(_TX,_TY){var _TZ=E(_TX);if(!_TZ[0]){return E(_TY);}else{var _U0=_TZ[1],_U1=E(_TY);if(!_U1[0]){return E(_TZ);}else{var _U2=_U1[1];return B(A(_TU,[_U0,_U2]))==2?[1,_U2,new T(function(){return B(_TW(_TZ,_U1[2]));})]:[1,_U0,new T(function(){return B(_TW(_TZ[2],_U1));})];}}},_U3=function(_U4){var _U5=E(_U4);if(!_U5[0]){return [0];}else{var _U6=E(_U5[2]);return _U6[0]==0?E(_U5):[1,new T(function(){return B(_TW(_U5[1],_U6[1]));}),new T(function(){return B(_U3(_U6[2]));})];}},_U7=function(_U8){while(1){var _U9=E(_U8);if(!_U9[0]){return E(new T(function(){return B(_U7(B(_U3(_1g))));}));}else{if(!E(_U9[2])[0]){return E(_U9[1]);}else{_U8=B(_U3(_U9));continue;}}}},_Ua=new T(function(){return B(_Ub(_1g));}),_Ub=function(_Uc){var _Ud=E(_Uc);if(!_Ud[0]){return E(_TS);}else{var _Ue=_Ud[1],_Uf=E(_Ud[2]);if(!_Uf[0]){return [1,_Ud,_1g];}else{var _Ug=_Uf[1],_Uh=_Uf[2];if(B(A(_TU,[_Ue,_Ug]))==2){return new F(function(){return (function(_Ui,_Uj,_Uk){while(1){var _Ul=(function(_Um,_Un,_Uo){var _Up=E(_Uo);if(!_Up[0]){return [1,[1,_Um,_Un],_Ua];}else{var _Uq=_Up[1];if(B(A(_TU,[_Um,_Uq]))==2){_Ui=_Uq;var _Ur=[1,_Um,_Un];_Uk=_Up[2];_Uj=_Ur;return null;}else{return [1,[1,_Um,_Un],new T(function(){return B(_Ub(_Up));})];}}})(_Ui,_Uj,_Uk);if(_Ul!=null){return _Ul;}}})(_Ug,[1,_Ue,_1g],_Uh);});}else{return new F(function(){return (function(_Us,_Ut,_Uu){while(1){var _Uv=(function(_Uw,_Ux,_Uy){var _Uz=E(_Uy);if(!_Uz[0]){return [1,new T(function(){return B(A(_Ux,[[1,_Uw,_1g]]));}),_Ua];}else{var _UA=_Uz[1],_UB=_Uz[2];switch(B(A(_TU,[_Uw,_UA]))){case 0:_Us=_UA;_Ut=function(_UC){return new F(function(){return A(_Ux,[[1,_Uw,_UC]]);});};_Uu=_UB;return null;case 1:_Us=_UA;_Ut=function(_UD){return new F(function(){return A(_Ux,[[1,_Uw,_UD]]);});};_Uu=_UB;return null;default:return [1,new T(function(){return B(A(_Ux,[[1,_Uw,_1g]]));}),new T(function(){return B(_Ub(_Uz));})];}}})(_Us,_Ut,_Uu);if(_Uv!=null){return _Uv;}}})(_Ug,function(_UE){return [1,_Ue,_UE];},_Uh);});}}}};return new F(function(){return _U7(B(_Ub(_TV)));});},_UF=function(_UG){return [0,new T(function(){return B(_TT(_TP,E(_UG)[1]));}),new T(function(){return E(E(_UG)[2]);})];},_UH=[0,1],_UI=new T(function(){return B(unCStr("Pattern match failure in do expression at Binary/Genetic.hs:54:3-14"));}),_UJ=new T(function(){return B(err(_UI));}),_UK=function(_UL){return E(_UJ);},_UM=new T(function(){return B(_UK(_));}),_UN=function(_UO,_UP){var _UQ=new T(function(){return [0,B(_IP(_UO,0))];});return function(_UR){return new F(function(){return _Sn(_R6,_nZ,_K,new T(function(){return B(_Sw(_R6,_QU,_UF,new T(function(){return B(A(new T(function(){var _US=function(_UT,_){return [1,new T(function(){var _UU=B(_SV(_Sk,_Rw,_UH,new T(function(){return B(_qN(E(_UQ)[1]-2|0));}),_UT));return [0,_UU[1],_UU[2]];})];},_UV=function(_UW){return _UW>1?function(_ay){return new F(function(){return _TG(_US,new T(function(){return B(_UV(_UW-1|0));}),_ay);});}:function(_9E){return new F(function(){return _TG(_US,_R7,_9E);});};};return B(_UV(3));}),[_UR]));})));}),function(_UX,_){var _UY=E(_UX),_UZ=E(_UY[1]);if(!_UZ[0]){return E(_UM);}else{var _V0=_UZ[1],_V1=E(_UZ[2]);if(!_V1[0]){return E(_UM);}else{var _V2=_V1[1],_V3=E(_V1[2]);if(!_V3[0]){return E(_UM);}else{var _V4=_V3[1];return E(_V3[2])[0]==0?[1,[0,[0,new T(function(){var _V5=E(_V0)[1];return B(_2J(B(_TB(0,_V5,_UO)),new T(function(){var _V6=E(_V2)[1];return B(_2J(B(_TB(_V5,_V6,_UP)),new T(function(){var _V7=E(_V4)[1];return B(_2J(B(_TB(_V6,_V7,_UO)),new T(function(){return B(_TB(_V7,E(_UQ)[1],_UP));},1)));},1)));},1)));}),new T(function(){var _V8=E(_V0)[1];return B(_2J(B(_TB(0,_V8,_UP)),new T(function(){var _V9=E(_V2)[1];return B(_2J(B(_TB(_V8,_V9,_UO)),new T(function(){var _Va=E(_V4)[1];return B(_2J(B(_TB(_V9,_Va,_UP)),new T(function(){return B(_TB(_Va,E(_UQ)[1],_UO));},1)));},1)));},1)));})],_UY[2]]]:E(_UM);}}}});});};},_Vb=function(_Vc,_Vd){var _Ve=B(_IP(_Vc,0));if(_Ve==B(_IP(_Vd,0))){if(_Ve>1){if(E(_Ve)==2){var _Vf=new T(function(){var _Vg=E(_Vc);if(!_Vg[0]){var _Vh=E(_QT);}else{var _Vi=E(_Vg[2]),_Vh=_Vi[0]==0?E(_QT):E(_Vi[2])[0]==0?[0,_Vg[1],_Vi[1]]:E(_QT);}return _Vh;}),_Vj=new T(function(){var _Vk=E(_Vd);if(!_Vk[0]){var _Vl=E(_QQ);}else{var _Vm=E(_Vk[2]),_Vl=_Vm[0]==0?E(_QQ):E(_Vm[2])[0]==0?[0,_Vk[1],_Vm[1]]:E(_QQ);}return _Vl;});return function(_Vn,_){return [1,[0,[0,[1,new T(function(){return E(E(_Vf)[1]);}),[1,new T(function(){return E(E(_Vj)[2]);}),_1g]],[1,new T(function(){return E(E(_Vj)[1]);}),[1,new T(function(){return E(E(_Vf)[2]);}),_1g]]],_Vn]];};}else{return new F(function(){return _UN(_Vc,_Vd);});}}else{return function(_Vo,_){return [1,[0,[0,_Vc,_Vd],_Vo]];};}}else{return E(_QJ);}},_Vp=function(_Vq,_Vr,_Vs){return new F(function(){return _Vb(_Vr,_Vs);});},_Vt=function(_Vu,_){return [1,[0,new T(function(){return E(E(_Vu)[1]);}),new T(function(){return E(E(_Vu)[2]);})]];},_Vv=[0,0],_Vw=[0,1],_Vx=function(_Vy){return [0,new T(function(){return !E(E(_Vy)[1])?E(_Vv):E(_Vw);}),new T(function(){return E(E(_Vy)[2]);})];},_Vz=function(_VA){return new F(function(){return _Sw(_R6,_QU,_Vx,function(_){return [1,new T(function(){var _VB=B(_SV(_Sk,_Rw,_SJ,_SE,_VA));return [0,new T(function(){return E(E(_VB[1])[1])==0?false:true;}),_VB[2]];})];});});},_VC=function(_VD,_VE,_VF){return new F(function(){return _Sn(_R6,_nZ,_K,new T(function(){return B(A(_VD,[_VF]));}),function(_VG){return new F(function(){return _Sn(_R6,_nZ,_K,new T(function(){return B(A(_VE,[new T(function(){return E(E(_VG)[2]);})]));}),function(_VH,_){return [1,[0,[1,new T(function(){return E(E(_VG)[1]);}),new T(function(){return E(E(_VH)[1]);})],new T(function(){return E(E(_VH)[2]);})]];});});});});},_VI=function(_9E){return new F(function(){return _VC(_Vz,_R7,_9E);});},_VJ=function(_VK){return _VK>1?function(_ay){return new F(function(){return _VC(_Vz,new T(function(){return B(_VJ(_VK-1|0));}),_ay);});}:E(_VI);},_VL=function(_VM){return function(_VN){return new F(function(){return _Sn(_R6,_nZ,_K,new T(function(){return B(A(new T(function(){var _VO=E(_VM)[1];return _VO>0?B(_VJ(_VO)):E(_R7);}),[_VN]));}),_Vt);});};},_VP=function(_VQ,_VR,_VS,_VT){return new F(function(){return _Ja(B(_qx(B(_qP(_VQ,_VT)),B(_qP(_VS,_VR)))),B(_qP(_VR,_VT)));});},_VU=function(_VV,_VW){var _VX=E(_VV),_VY=E(_VW),_VZ=B(_VP(_VX[1],_VX[2],_VY[1],_VY[2]));return [0,E(_VZ[1]),E(_VZ[2])];},_W0=function(_W1,_W2,_W3,_W4){return new F(function(){return _Ja(B(_3y(B(_qP(_W1,_W4)),B(_qP(_W3,_W2)))),B(_qP(_W2,_W4)));});},_W5=function(_W6,_W7){var _W8=E(_W6),_W9=E(_W7),_Wa=B(_W0(_W8[1],_W8[2],_W9[1],_W9[2]));return [0,E(_Wa[1]),E(_Wa[2])];},_Wb=function(_Wc,_Wd){var _We=E(_Wc),_Wf=E(_Wd),_Wg=B(_Ja(B(_qP(_We[1],_Wf[1])),B(_qP(_We[2],_Wf[2]))));return [0,E(_Wg[1]),E(_Wg[2])];},_Wh=function(_Wi){var _Wj=E(_Wi);return [0,E(B(_J2(_Wj[1]))),E(_Wj[2])];},_Wk=function(_Wl){return [0,E(E(_Wl)),E(_Je)];},_Wm=function(_Wn){var _Wo=E(_Wn);return [0,E(B(_qH(_Wo[1]))),E(_Wo[2])];},_Wp=function(_Wq){return [0,E(B(_Jz(E(_Wq)[1]))),E(_Je)];},_Wr=[0,_VU,_Wb,_W5,_Wm,_Wh,_Wp,_Wk],_Ws=function(_Wt,_Wu){var _Wv=E(_Wt),_Ww=E(_Wu);return new F(function(){return _4d(B(_qP(_Wv[1],_Ww[2])),B(_qP(_Ww[1],_Wv[2])));});},_Wx=function(_Wy){var _Wz=E(_Wy);return new F(function(){return _IM(_Wz[1],_Wz[2]);});},_WA=function(_WB){return [0,1/E(_WB)[1]];},_WC=function(_WD){var _WE=E(_WD),_WF=_WE[1];return _WF<0?[0, -_WF]:E(_WE);},_WG=function(_WH){var _WI=E(_WH);return _WI[0]==0?_WI[1]:I_toNumber(_WI[1]);},_WJ=function(_WK){return [0,B(_WG(_WK))];},_WL=[0,0],_WM=[0,1],_WN=[0,-1],_WO=function(_WP){var _WQ=E(E(_WP)[1]);return _WQ==0?E(_WL):_WQ<=0?E(_WN):E(_WM);},_WR=function(_WS,_WT){return [0,E(_WS)[1]-E(_WT)[1]];},_WU=function(_WV){return [0, -E(_WV)[1]];},_WW=function(_WX,_WY){return [0,E(_WX)[1]+E(_WY)[1]];},_WZ=function(_X0,_X1){return [0,E(_X0)[1]*E(_X1)[1]];},_X2=[0,_WW,_WZ,_WR,_WU,_WC,_WO,_WJ],_X3=function(_X4,_X5){return [0,E(_X4)[1]/E(_X5)[1]];},_X6=[0,_X2,_X3,_WA,_Wx],_X7=function(_X8,_X9){return E(_X8)[1]!=E(_X9)[1]?true:false;},_Xa=function(_Xb,_Xc){return E(_Xb)[1]==E(_Xc)[1];},_Xd=[0,_Xa,_X7],_Xe=function(_Xf,_Xg){return E(_Xf)[1]<E(_Xg)[1];},_Xh=function(_Xi,_Xj){return E(_Xi)[1]<=E(_Xj)[1];},_Xk=function(_Xl,_Xm){return E(_Xl)[1]>E(_Xm)[1];},_Xn=function(_Xo,_Xp){return E(_Xo)[1]>=E(_Xp)[1];},_Xq=function(_Xr,_Xs){var _Xt=E(_Xr)[1],_Xu=E(_Xs)[1];return _Xt>=_Xu?_Xt!=_Xu?2:1:0;},_Xv=function(_Xw,_Xx){var _Xy=E(_Xw),_Xz=E(_Xx);return _Xy[1]>_Xz[1]?E(_Xy):E(_Xz);},_XA=function(_XB,_XC){var _XD=E(_XB),_XE=E(_XC);return _XD[1]>_XE[1]?E(_XE):E(_XD);},_XF=[0,_Xd,_Xq,_Xe,_Xn,_Xk,_Xh,_Xv,_XA],_XG=function(_XH,_XI){var _XJ=hs_timesInt64(E(_XH)[1],E(_XI)[1]),_XK=_XJ;return [0,_XK];},_XL=function(_XM,_XN){var _XO=hs_plusInt64(E(_XM)[1],E(_XN)[1]),_XP=_XO;return [0,_XP];},_XQ=function(_XR,_XS){var _XT=hs_minusInt64(E(_XR)[1],E(_XS)[1]),_XU=_XT;return [0,_XU];},_XV=function(_XW){var _XX=hs_geInt64(_XW,new Long(0,0)),_XY=_XX;if(!E(_XY)){var _XZ=hs_negateInt64(_XW),_Y0=_XZ;return E(_Y0);}else{return E(_XW);}},_Y1=function(_Y2){return [0,B(_XV(E(_Y2)[1]))];},_Y3=function(_Y4){var _Y5=hs_intToInt64(_Y4),_Y6=_Y5;return E(_Y6);},_Y7=function(_Y8){var _Y9=E(_Y8);return _Y9[0]==0?B(_Y3(_Y9[1])):I_toInt64(_Y9[1]);},_Ya=function(_Yb){return [0,B(_Y7(_Yb))];},_Yc=function(_Yd){var _Ye=hs_negateInt64(E(_Yd)[1]),_Yf=_Ye;return [0,_Yf];},_Yg=[0,new Long(1,0)],_Yh=new T(function(){var _Yi=hs_negateInt64(new Long(1,0)),_Yj=_Yi;return [0,_Yj];}),_Yk=[0,new Long(0,0)],_Yl=function(_Ym){var _Yn=hs_gtInt64(_Ym,new Long(0,0)),_Yo=_Yn;if(!E(_Yo)){var _Yp=hs_eqInt64(_Ym,new Long(0,0)),_Yq=_Yp;return E(_Yq)==0?E(_Yh):E(_Yk);}else{return E(_Yg);}},_Yr=function(_Ys){return new F(function(){return _Yl(E(_Ys)[1]);});},_Yt=[0,_XL,_XG,_XQ,_Yc,_Y1,_Yr,_Ya],_Yu=[1,I_fromBits([4294967295,2147483647])],_Yv=new T(function(){var _Yw=hs_negateInt64(new Long(0,-2147483648)),_Yx=_Yw;return [0,_Yx];}),_Yy=function(_Yz){var _YA=hs_intToInt64(2147483647),_YB=_YA,_YC=hs_leInt64(_Yz,_YB),_YD=_YC;if(!E(_YD)){return [1,I_fromInt64(_Yz)];}else{var _YE=hs_intToInt64(-2147483648),_YF=_YE,_YG=hs_geInt64(_Yz,_YF),_YH=_YG;if(!E(_YH)){return [1,I_fromInt64(_Yz)];}else{var _YI=hs_int64ToInt(_Yz),_YJ=_YI;return new F(function(){return _qN(_YJ);});}}},_YK=new T(function(){return B(_Yy(E(_Yv)[1]));}),_YL=[0,new Long(2,0)],_YM=[0,new Long(1,0)],_YN=new T(function(){return B(unCStr("Negative exponent"));}),_YO=new T(function(){return B(err(_YN));}),_YP=0,_YQ=new T(function(){return B(_3f(_YP));}),_YR=new T(function(){return die(_YQ);}),_YS=function(_YT,_YU){var _YV=hs_eqInt64(_YU,new Long(0,0)),_YW=_YV;if(!E(_YW)){var _YX=hs_eqInt64(_YU,E(_Yh)[1]),_YY=_YX;if(!E(_YY)){var _YZ=hs_quotInt64(_YT,_YU),_Z0=_YZ;return E(_Z0);}else{var _Z1=hs_eqInt64(_YT,E(_Yv)[1]),_Z2=_Z1;if(!E(_Z2)){var _Z3=hs_quotInt64(_YT,_YU),_Z4=_Z3;return E(_Z4);}else{return E(_YR);}}}else{return E(_3i);}},_Z5=function(_Z6,_Z7){var _Z8=hs_eqInt64(_Z7,new Long(0,0)),_Z9=_Z8;if(!E(_Z9)){var _Za=hs_eqInt64(_Z7,E(_Yh)[1]),_Zb=_Za;if(!E(_Zb)){var _Zc=hs_remInt64(_Z6,_Z7),_Zd=_Zc;return E(_Zd);}else{return new F(function(){return new Long(0,0);});}}else{return E(_3i);}},_Ze=function(_Zf,_Zg,_Zh){while(1){var _Zi=(function(_Zj,_Zk,_Zl){var _Zm=hs_eqInt64(B(_Z5(_Zk,new Long(2,0))),new Long(0,0)),_Zn=_Zm;if(!E(_Zn)){var _Zo=hs_eqInt64(_Zk,new Long(1,0)),_Zp=_Zo;if(!E(_Zp)){var _Zq=hs_minusInt64(_Zk,new Long(1,0)),_Zr=_Zq;_Zf=new T(function(){return B(_XG(_Zj,_Zj));});_Zg=B(_YS(_Zr,new Long(2,0)));_Zh=new T(function(){return B(_XG(_Zj,_Zl));},1);return null;}else{var _Zs=hs_timesInt64(E(_Zj)[1],E(_Zl)[1]),_Zt=_Zs;return E(_Zt);}}else{_Zf=new T(function(){return B(_XG(_Zj,_Zj));});var _Zu=B(_YS(_Zk,new Long(2,0))),_Zv=_Zl;_Zg=_Zu;_Zh=_Zv;return null;}})(_Zf,_Zg,_Zh);if(_Zi!=null){return _Zi;}}},_Zw=function(_Zx,_Zy){while(1){var _Zz=(function(_ZA,_ZB){var _ZC=hs_eqInt64(B(_Z5(_ZB,new Long(2,0))),new Long(0,0)),_ZD=_ZC;if(!E(_ZD)){var _ZE=hs_eqInt64(_ZB,new Long(1,0)),_ZF=_ZE;if(!E(_ZF)){var _ZG=hs_minusInt64(_ZB,new Long(1,0)),_ZH=_ZG;return [0,B(_Ze(new T(function(){return B(_XG(_ZA,_ZA));}),B(_YS(_ZH,new Long(2,0))),_ZA))];}else{return E(_ZA);}}else{_Zx=new T(function(){return B(_XG(_ZA,_ZA));});var _ZI=B(_YS(_ZB,new Long(2,0)));_Zy=_ZI;return null;}})(_Zx,_Zy);if(_Zz!=null){return _Zz;}}},_ZJ=function(_ZK,_ZL){var _ZM=hs_ltInt64(_ZL,new Long(0,0)),_ZN=_ZM;if(!E(_ZN)){var _ZO=hs_eqInt64(_ZL,new Long(0,0)),_ZP=_ZO;return E(_ZP)==0?B(_Zw(_ZK,_ZL)):E(_YM);}else{return E(_YO);}},_ZQ=new T(function(){return B(_ZJ(_YL,new Long(53,0)));}),_ZR=new T(function(){return [0,B(_WG(B(_Yy(E(_ZQ)[1]))))];}),_ZS=new T(function(){var _ZT=hs_minusInt64(E(_ZQ)[1],new Long(1,0)),_ZU=_ZT;return [0,_ZU];}),_ZV=function(_ZW,_ZX){var _ZY=hs_int64ToWord64(_ZX),_ZZ=_ZY,_100=hs_int64ToWord64(_ZW),_101=_100,_102=hs_and64(_101,_ZZ),_103=_102,_104=hs_word64ToInt64(_103),_105=_104;return E(_105);},_106=function(_107,_108){return [0,E(_107)[1],E(_108)[1]];},_109=function(_10a,_10b){var _10c=quot(_10b,52774),_10d=(imul(40692,_10b-(imul(_10c,52774)|0)|0)|0)-(imul(_10c,3791)|0)|0,_10e=new T(function(){if(_10d>=0){var _10f=[0,_10d];}else{var _10f=[0,_10d+2147483399|0];}var _10g=_10f;return _10g;}),_10h=quot(_10a,53668),_10i=(imul(40014,_10a-(imul(_10h,53668)|0)|0)|0)-(imul(_10h,12211)|0)|0,_10j=new T(function(){if(_10i>=0){var _10k=[0,_10i];}else{var _10k=[0,_10i+2147483563|0];}var _10l=_10k;return _10l;});return [0,new T(function(){var _10m=E(_10j)[1]-E(_10e)[1]|0;if(_10m>=1){var _10n=[0,_10m];}else{var _10n=[0,_10m+2147483562|0];}var _10o=_10n,_10p=_10o,_10q=_10p,_10r=_10q;return _10r;}),new T(function(){return B(_106(_10j,_10e));})];},_10s=[0,2147483562],_10t=function(_10u,_10v,_10w,_10x){while(1){var _10y=(function(_10z,_10A,_10B,_10C){if(!B(_K9(_10A,_10B))){var _10D=B(_qx(B(_3y(_10B,_10A)),_SE)),_10E=B((function(_10F,_10G,_10H){while(1){if(!B(_JM(_10F,B(_qP(_10D,_SK))))){var _10I=E(_10H),_10J=B(_109(_10I[1],_10I[2])),_10K=B(_qP(_10F,_10s)),_10L=B(_qx(B(_qP(_10G,_10s)),B(_3y(B(_qN(E(_10J[1])[1])),_SE))));_10H=_10J[2];_10F=_10K;_10G=_10L;continue;}else{return [0,_10G,_10H];}}})(_SE,_SJ,_10C));return [0,new T(function(){return B(A(_SF,[_10z,new T(function(){if(!B(_3j(_10D,_SJ))){var _10M=B(_qx(_10A,B(_SL(_10E[1],_10D))));}else{var _10M=E(_3i);}return _10M;})]));}),_10E[2]];}else{var _10N=_10z,_10O=_10B,_10P=_10A,_10Q=_10C;_10u=_10N;_10v=_10O;_10w=_10P;_10x=_10Q;return null;}})(_10u,_10v,_10w,_10x);if(_10y!=null){return _10y;}}},_10R=function(_10S){var _10T=B(_10t(_Yt,_YK,_Yu,_10S));return [0,new T(function(){return [0,B(_WG(B(_Yy(B(_ZV(E(_ZS)[1],E(_10T[1])[1]))))))/E(_ZR)[1]];}),_10T[2]];},_10U=function(_10V){var _10W=new T(function(){var _10X=B(_10R(_10V));return [0,_10X[2],_10X[1]];}),_10Y=new T(function(){return E(E(_10W)[1]);});return [0,_10Y,new T(function(){var _10Z=E(_10Y);return E(E(_10W)[2]);})];},_110=[1,I_fromBits([3567587328,232])],_111=function(_112,_113,_114,_115){var _116=B(_qP(_113,_114));return new F(function(){return _Ja(B(_qP(B(_qP(_112,_115)),B(_Jz(_116)))),B(_J2(_116)));});},_117=[0,0],_118=function(_119,_11a){var _11b=E(_11a);if(!_11b){return E(_3i);}else{var _11c=function(_11d){if(_119<=0){if(_119>=0){var _11e=quotRemI(_119,_11b);return [0,[0,_11e[1]],[0,_11e[2]]];}else{if(_11b<=0){var _11f=quotRemI(_119,_11b);return [0,[0,_11f[1]],[0,_11f[2]]];}else{var _11g=quotRemI(_119+1|0,_11b);return [0,[0,_11g[1]-1|0],[0,(_11g[2]+_11b|0)-1|0]];}}}else{if(_11b>=0){if(_119>=0){var _11h=quotRemI(_119,_11b);return [0,[0,_11h[1]],[0,_11h[2]]];}else{if(_11b<=0){var _11i=quotRemI(_119,_11b);return [0,[0,_11i[1]],[0,_11i[2]]];}else{var _11j=quotRemI(_119+1|0,_11b);return [0,[0,_11j[1]-1|0],[0,(_11j[2]+_11b|0)-1|0]];}}}else{var _11k=quotRemI(_119-1|0,_11b);return [0,[0,_11k[1]-1|0],[0,(_11k[2]+_11b|0)+1|0]];}}};return E(_11b)==(-1)?E(_119)==(-2147483648)?[0,_YR,_117]:B(_11c(_)):B(_11c(_));}},_11l=function(_11m){var _11n=B(_118((_11m>>>0&2147483647>>>0)>>>0&4294967295,2147483562));return [0,E(_11n[2])[1]+1|0,B(_RN(E(_11n[1])[1],2147483398))+1|0];},_11o=function(_11p){return E(_110);},_11q=[0,1],_11r=function(_11s,_11t){var _11u=E(_11s);return [0,_11u,new T(function(){var _11v=B(_11r(B(_qx(_11u,_11t)),_11t));return [1,_11v[1],_11v[2]];})];},_11w=function(_11x){var _11y=B(_11r(_11x,_11q));return [1,_11y[1],_11y[2]];},_11z=function(_11A,_11B){var _11C=B(_11r(_11A,new T(function(){return B(_3y(_11B,_11A));})));return [1,_11C[1],_11C[2]];},_11D=[0,0],_11E=function(_11F,_11G,_11H){if(!B(_JM(_11G,_11D))){var _11I=function(_11J){return !B(_4d(_11J,_11H))?[1,_11J,new T(function(){return B(_11I(B(_qx(_11J,_11G))));})]:[0];};return new F(function(){return _11I(_11F);});}else{var _11K=function(_11L){return !B(_K9(_11L,_11H))?[1,_11L,new T(function(){return B(_11K(B(_qx(_11L,_11G))));})]:[0];};return new F(function(){return _11K(_11F);});}},_11M=function(_11N,_11O,_11P){return new F(function(){return _11E(_11N,B(_3y(_11O,_11N)),_11P);});},_11Q=function(_11R,_11S){return new F(function(){return _11E(_11R,_11q,_11S);});},_11T=function(_11U){return [0,B(_s2(_11U))];},_11V=function(_11W){return new F(function(){return _3y(_11W,_11q);});},_11X=function(_11Y){return new F(function(){return _qx(_11Y,_11q);});},_11Z=function(_120){return new F(function(){return _qN(E(_120)[1]);});},_121=[0,_11X,_11V,_11Z,_11T,_11w,_11z,_11Q,_11M],_122=function(_123,_124){if(_123<=0){if(_123>=0){return new F(function(){return quot(_123,_124);});}else{if(_124<=0){return new F(function(){return quot(_123,_124);});}else{return quot(_123+1|0,_124)-1|0;}}}else{if(_124>=0){if(_123>=0){return new F(function(){return quot(_123,_124);});}else{if(_124<=0){return new F(function(){return quot(_123,_124);});}else{return quot(_123+1|0,_124)-1|0;}}}else{return quot(_123-1|0,_124)-1|0;}}},_125=function(_126,_127){while(1){var _128=E(_126);if(!_128[0]){var _129=E(_128[1]);if(_129==(-2147483648)){_126=[1,I_fromInt(-2147483648)];continue;}else{var _12a=E(_127);if(!_12a[0]){return [0,B(_122(_129,_12a[1]))];}else{_126=[1,I_fromInt(_129)];_127=_12a;continue;}}}else{var _12b=_128[1],_12c=E(_127);return _12c[0]==0?[0,I_toInt(I_div(_12b,I_fromInt(_12c[1])))]:[1,I_div(_12b,_12c[1])];}}},_12d=function(_12e,_12f){return !B(_3j(_12f,_3r))?B(_125(_12e,_12f)):E(_3i);},_12g=function(_12h,_12i){while(1){var _12j=E(_12h);if(!_12j[0]){var _12k=E(_12j[1]);if(_12k==(-2147483648)){_12h=[1,I_fromInt(-2147483648)];continue;}else{var _12l=E(_12i);if(!_12l[0]){var _12m=_12l[1];return [0,[0,B(_122(_12k,_12m))],[0,B(_RN(_12k,_12m))]];}else{_12h=[1,I_fromInt(_12k)];_12i=_12l;continue;}}}else{var _12n=E(_12i);if(!_12n[0]){_12h=_12j;_12i=[1,I_fromInt(_12n[1])];continue;}else{var _12o=I_divMod(_12j[1],_12n[1]);return [0,[1,_12o[1]],[1,_12o[2]]];}}}},_12p=function(_12q,_12r){if(!B(_3j(_12r,_3r))){var _12s=B(_12g(_12q,_12r));return [0,_12s[1],_12s[2]];}else{return E(_3i);}},_12t=function(_12u,_12v){return !B(_3j(_12v,_3r))?B(_SL(_12u,_12v)):E(_3i);},_12w=function(_12x,_12y){return !B(_3j(_12y,_3r))?B(_3H(_12x,_12y)):E(_3i);},_12z=function(_12A,_12B){while(1){var _12C=E(_12A);if(!_12C[0]){var _12D=E(_12C[1]);if(_12D==(-2147483648)){_12A=[1,I_fromInt(-2147483648)];continue;}else{var _12E=E(_12B);if(!_12E[0]){var _12F=_12E[1];return [0,[0,quot(_12D,_12F)],[0,_12D%_12F]];}else{_12A=[1,I_fromInt(_12D)];_12B=_12E;continue;}}}else{var _12G=E(_12B);if(!_12G[0]){_12A=_12C;_12B=[1,I_fromInt(_12G[1])];continue;}else{var _12H=I_quotRem(_12C[1],_12G[1]);return [0,[1,_12H[1]],[1,_12H[2]]];}}}},_12I=function(_12J,_12K){if(!B(_3j(_12K,_3r))){var _12L=B(_12z(_12J,_12K));return [0,_12L[1],_12L[2]];}else{return E(_3i);}},_12M=function(_12N){return E(_12N);},_12O=function(_12P){return E(_12P);},_12Q=[0,_qx,_qP,_3y,_qH,_J2,_Jz,_12O],_12R=function(_12S,_12T){var _12U=E(_12S);if(!_12U[0]){var _12V=_12U[1],_12W=E(_12T);return _12W[0]==0?_12V!=_12W[1]:I_compareInt(_12W[1],_12V)==0?false:true;}else{var _12X=_12U[1],_12Y=E(_12T);return _12Y[0]==0?I_compareInt(_12X,_12Y[1])==0?false:true:I_compare(_12X,_12Y[1])==0?false:true;}},_12Z=[0,_3j,_12R],_130=function(_131,_132){return !B(_s5(_131,_132))?E(_131):E(_132);},_133=function(_134,_135){return !B(_s5(_134,_135))?E(_135):E(_134);},_136=function(_137,_138){var _139=E(_137);if(!_139[0]){var _13a=_139[1],_13b=E(_138);if(!_13b[0]){var _13c=_13b[1];return _13a!=_13c?_13a>_13c?2:0:1;}else{var _13d=I_compareInt(_13b[1],_13a);return _13d<=0?_13d>=0?1:2:0;}}else{var _13e=_139[1],_13f=E(_138);if(!_13f[0]){var _13g=I_compareInt(_13e,_13f[1]);return _13g>=0?_13g<=0?1:2:0;}else{var _13h=I_compare(_13e,_13f[1]);return _13h>=0?_13h<=0?1:2:0;}}},_13i=[0,_12Z,_136,_4d,_JM,_K9,_s5,_130,_133],_13j=[0,_12Q,_13i,_Wk],_13k=[0,_13j,_121,_12w,_IU,_12d,_12t,_12I,_12p,_12M],_13l=[0,0],_13m=function(_13n,_13o,_13p){var _13q=B(A(_13n,[_13o]));if(!B(_3j(_13q,_13l))){return new F(function(){return _125(B(_qP(_13o,_13p)),_13q);});}else{return E(_3i);}},_13r=function(_13s){return E(E(_13s)[1]);},_13t=function(_13u){return E(E(_13u)[1]);},_13v=function(_13w,_13x,_13y){var _13z=new T(function(){if(!B(_3j(_13y,_3r))){var _13A=B(_12z(_13x,_13y)),_13B=[0,_13A[1],_13A[2]];}else{var _13B=E(_3i);}return _13B;});return [0,new T(function(){return B(A(_SF,[B(_13t(B(_13r(_13w)))),new T(function(){return E(E(_13z)[1]);})]));}),new T(function(){return [0,E(E(E(_13z)[2])),E(_13y)];})];},_13C=function(_13D,_13E,_13F){var _13G=B(_13v(_13D,_13E,_13F)),_13H=_13G[1],_13I=E(_13G[2]);if(!B(_4d(B(_qP(_13I[1],_Je)),B(_qP(_3r,_13I[2]))))){return E(_13H);}else{var _13J=E(B(_13r(_13D))[1]);return new F(function(){return A(_13J[3],[_13H,new T(function(){return B(A(_13J[7],[_Je]));})]);});}},_13K=[1,I_fromBits([2627207168,20116567])],_13L=[0,40587],_13M=function(_13N){var _13O=new T(function(){var _13P=B(_111(E(_13N),_Je,_110,_Je)),_13Q=B(_111(_13K,_Je,_110,_Je)),_13R=B(_111(_13P[1],_13P[2],_13Q[1],_13Q[2]));return B(_13C(_13k,_13R[1],_13R[2]));});return [0,new T(function(){return B(_qx(_13L,_13O));}),new T(function(){return B(_3y(_13N,B(_13m(_11o,B(_qP(_13O,_110)),_13K))));})];},_13S=[0,0],_13T=function(_13U,_13V,_){var _=writeOffAddr("w32",4,E(_13U)[1],0,E(_13V)[1]);return _c;},_13W=function(_13X,_){var _13Y=readOffAddr("w32",4,E(_13X)[1],0),_13Z=_13Y;return [0,_13Z];},_140=function(_141,_142,_143,_){var _=writeOffAddr("w32",4,plusAddr(E(_141)[1],E(_142)[1]),0,E(_143)[1]);return _c;},_144=function(_145,_146,_){var _147=readOffAddr("w32",4,plusAddr(E(_145)[1],E(_146)[1]),0),_148=_147;return [0,_148];},_149=[0,4],_14a=function(_14b){return E(_149);},_14c=function(_14d,_14e,_){var _14f=readOffAddr("w32",4,E(_14d)[1],E(_14e)[1]),_14g=_14f;return [0,_14g];},_14h=function(_14i,_14j,_14k,_){var _=writeOffAddr("w32",4,E(_14i)[1],E(_14j)[1],E(_14k)[1]);return _c;},_14l=[0,_14a,_14a,_14c,_14h,_144,_140,_13W,_13T],_14m=[0,0],_14n=function(_14o){return E(E(_14o)[3]);},_14p=function(_14q,_14r,_14s,_){if(_14r>0){return new F(function(){return (function(_14t,_14u,_){while(1){var _14v=E(_14t);if(!_14v){var _14w=B(A(new T(function(){return B(A(_14n,[_14q,_14s,_14m]));}),[_])),_14x=_14w;return [1,_14x,_14u];}else{var _14y=B(A(new T(function(){return B(_14n(_14q));}),[_14s,[0,_14v],_])),_14z=_14y;_14t=_14v-1|0;var _14A=[1,_14z,_14u];_14u=_14A;continue;}}})(_14r-1|0,_1g,_);});}else{return _1g;}},_14B=0,_14C=1,_14D=function(_14E,_14F,_14G,_){var _14H=0,_14I=_14H;switch(E(_14I)){case 0:return new F(function(){return (function(_){var _14J=B(A(_14E,[_])),_14K=_14J,_14L=jsCatch(function(_){return new F(function(){return new T(function(){return B(A(_14G,[_14K]));})();});},function(_14M,_){var _14N=B(A(_14F,[_14K,_])),_14O=_14N;return new F(function(){return die(_14M);});}),_14P=_14L,_14Q=B(A(_14F,[_14K,_])),_14R=_14Q;return _14P;})();});break;case 1:var _14S=B(A(_14E,[_])),_14T=_14S,_14U=jsCatch(new T(function(){return B(A(_14G,[_14T]));}),function(_14V,_){var _14W=B(A(_14F,[_14T,_])),_14X=_14W;return new F(function(){return die(_14V);});}),_14Y=_14U,_14Z=B(A(_14F,[_14T,_])),_150=_14Z;return _14Y;default:var _151=B(A(_14E,[_])),_152=_151,_153=jsCatch(new T(function(){return B(A(_14G,[_152]));}),function(_154,_){var _155=B(A(_14F,[_152,_])),_156=_155;return new F(function(){return die(_154);});}),_157=_153,_158=B(A(_14F,[_152,_])),_159=_158;return _157;}},_15a=function(_15b){return E(E(_15b)[3]);},_15c=0,_15d=[0,_15c,_1g],_15e=new T(function(){return B(unCStr("mallocForeignPtrBytes: size must be >= 0"));}),_15f=new T(function(){return B(err(_15e));}),_15g=function(_15h,_15i,_){var _15j=B((function(_15k,_){while(1){var _15l=readOffAddr("i8",1,_15i,_15k),_15m=_15l;if(!E(_15m)){return [0,_15k];}else{var _15n=_15k+1|0;_15k=_15n;continue;}}})(0,_)),_15o=_15j;return new F(function(){return _14D(E(_15h)[2],_15a,function(_15p,_){var _15q=nMV(_15d),_15r=_15q,_15s=E(_15o)[1],_15t=function(_15u){var _15v=imul(_15u,4)|0;if(_15v>=0){var _15w=nMV(_15d),_15x=_15w,_15y=newByteArr(_15v),_15z=_15y,_15A=function(_15B,_){var _15C=E(_15p),_15D=B(A(_15C[1],[_15B,[0,_15z,[1,_15z,_15x],_14C,_15u,0,0],_])),_15E=_15D,_15F=E(_15E),_15G=_15F[3],_15H=E(_15F[2]);if(_15H[5]!=_15H[6]){if(E(_15F[1])==1){var _15I=E(_15G),_15J=_15I[2],_15K=B(_14p(_14l,_15I[6]-_15I[5]|0,[0,_15I[1]],_)),_15L=_15K,_=0,_15M=B(_15A(_15H,_)),_15N=_15M;return new T(function(){return B(_2J(_15L,_15N));});}else{var _15O=B(A(_15C[2],[_15H,_15G,_])),_15P=_15O,_15Q=E(_15P),_15R=E(_15Q[2]),_15S=_15R[2],_15T=B(_14p(_14l,_15R[6]-_15R[5]|0,[0,_15R[1]],_)),_15U=_15T,_=0,_15V=B(_15A(_15Q[1],_)),_15W=_15V;return new T(function(){return B(_2J(_15U,_15W));});}}else{var _15X=E(_15G),_15Y=_15X[2],_15Z=B(_14p(_14l,_15X[6]-_15X[5]|0,[0,_15X[1]],_)),_160=_15Z,_=0;return _160;}};return new F(function(){return _15A([0,_15i,[0,_15r],_14B,_15s,0,_15s],_);});}else{return E(_15f);}};return _15s>1?B(_15t(_15s)):B(_15t(1));},_);});},_161=1,_162=new T(function(){return B(unCStr("UTF16LE"));}),_163=new T(function(){return B(unCStr("UTF16BE"));}),_164=new T(function(){return B(unCStr("UTF16"));}),_165=new T(function(){return B(unCStr("UTF8"));}),_166=new T(function(){return B(unCStr("UTF32LE"));}),_167=new T(function(){return B(unCStr("UTF32BE"));}),_168=new T(function(){return B(unCStr("UTF32"));}),_169=function(_16a){var _16b=u_towupper(_16a),_16c=_16b;return _16c>>>0>1114111?B(_s0(_16c)):_16c;},_16d=function(_16e){while(1){var _16f=(function(_16g){var _16h=E(_16g);if(!_16h[0]){return [0];}else{var _16i=_16h[2],_16j=E(E(_16h[1])[1]);if(_16j==45){_16e=_16i;return null;}else{return [1,new T(function(){return [0,B(_169(_16j))];}),new T(function(){return B(_16d(_16i));})];}}})(_16e);if(_16f!=null){return _16f;}}},_16k=new T(function(){return B(unCStr("UTF-32LE"));}),_16l=0,_16m=1,_16n=new T(function(){return [0, -(1&4294967295)>>>0];}),_16o=[0,0],_16p=function(_16q,_){return new F(function(){return die(new T(function(){return B(_nM(_16q));}));});},_16r=function(_16s,_){return new F(function(){return _16p(_16s,_);});},_16t=new T(function(){return B(unCStr("iconvRecoder"));}),_16u=[0,-1],_16v=function(_16w,_16x,_16y,_16z,_16A,_16B,_16C,_16D,_16E,_16F,_16G,_16H,_16I,_16J,_16K,_){var _16L=newByteArr(4),_16M=_16L,_16N=_16M,_16O=_16N,_16P=E(_16D)[1],_16Q=function(_16R){var _16S=plusAddr(_16x,_16R),_=die("Unsupported PrimOp: writeAddrOffAddr#"),_16T=newByteArr(4),_16U=_16T,_16V=_16U,_16W=_16V,_16X=E(_16K)[1],_16Y=function(_16Z){var _170=plusAddr(_16E,_16Z),_=die("Unsupported PrimOp: writeAddrOffAddr#"),_171=newByteArr(4),_172=_171,_173=_172,_174=_173,_175=function(_176){var _177=_174,_=writeOffAddr("w32",4,_177,0,_176),_178=newByteArr(4),_179=_178,_17a=_179,_17b=_17a,_17c=function(_17d){var _17e=_17b,_=writeOffAddr("w32",4,_17e,0,_17d),_17f=hs_iconv(E(_16w)[1],_16O,_177,_16W,_17e),_17g=_17f,_17h=readOffAddr("w32",4,_177,0),_17i=_17h,_17j=readOffAddr("w32",4,_17e,0),_17k=_17j,_17l=new T(function(){if(_16X<32){var _17m=[0,(_17k&4294967295)>>_16X];}else{var _17m=(_17k&4294967295)>=0?E(_16o):E(_16u);}var _17n=_17m;return _17n;}),_17o=new T(function(){var _17p=E(_17i);if(!_17p){var _17q=[0,_16x,_16y,_16z,_16A,0,0];}else{if(_16P<32){var _17r=[0,_16x,_16y,_16z,_16A,_16C-((_17p&4294967295)>>_16P)|0,_16C];}else{if((_17p&4294967295)>=0){var _17s=[0,_16x,_16y,_16z,_16A,_16C,_16C];}else{var _17s=[0,_16x,_16y,_16z,_16A,_16C+1|0,_16C];}var _17t=_17s,_17u=_17t,_17r=_17u;}var _17v=_17r,_17q=_17v;}return _17q;});if(_17g!=E(_16n)[1]){var _=0,_=0,_=0,_=0,_=0,_=0;return [0,_16l,_17o,new T(function(){return [0,_16E,_16F,_16G,_16H,_16I,_16H-E(_17l)[1]|0];})];}else{var _17w=__hscore_get_errno(),_17x=_17w;switch(E(_17x)){case 7:var _=0,_=0,_=0,_=0,_=0,_=0;return [0,_16m,_17o,new T(function(){return [0,_16E,_16F,_16G,_16H,_16I,_16H-E(_17l)[1]|0];})];case 22:var _=0,_=0,_=0,_=0,_=0,_=0;return [0,_16l,_17o,new T(function(){return [0,_16E,_16F,_16G,_16H,_16I,_16H-E(_17l)[1]|0];})];case 84:var _=0,_=0,_=0,_=0,_=0,_=0;return [0,new T(function(){return E(E(_17l)[1])==0?1:2;}),_17o,new T(function(){return [0,_16E,_16F,_16G,_16H,_16I,_16H-E(_17l)[1]|0];})];default:var _17y=__hscore_get_errno(),_17z=_17y;return new F(function(){return _16r(B(_17A(_16t,_17z,_5h,_5h)),_);});}}};if(_16X<32){return new F(function(){return _17c((_16H-_16J|0)<<_16X>>>0);});}else{return new F(function(){return _17c(0);});}};if(_16P<32){return new F(function(){return _175((_16C-_16B|0)<<_16P>>>0);});}else{return new F(function(){return _175(0);});}};if(_16X<32){return new F(function(){return _16Y(_16J<<_16X);});}else{return new F(function(){return _16Y(0);});}};if(_16P<32){return new F(function(){return _16Q(_16B<<_16P);});}else{return new F(function(){return _16Q(0);});}},_17B=[0,2],_17C=function(_17D,_17E,_17F,_){var _17G=E(_17E),_17H=E(_17F);return new F(function(){return _16v(_17D,_17G[1],_17G[2],_17G[3],_17G[4],_17G[5],_17G[6],_17B,_17H[1],_17H[2],_17H[3],_17H[4],_17H[5],_17H[6],_16o,_);});},_17I=function(_17J,_17K,_17L,_){var _17M=E(_17K),_17N=E(_17L);return new F(function(){return _16v(_17J,_17M[1],_17M[2],_17M[3],_17M[4],_17M[5],_17M[6],_16o,_17N[1],_17N[2],_17N[3],_17N[4],_17N[5],_17N[6],_17B,_);});},_17O=function(_17P){return E(E(_17P)[1])==47?false:true;},_17Q=function(_17R,_){return _c;},_17S=function(_){return _c;},_17T=new T(function(){return B(unCStr("mkTextEncoding"));}),_17U=new T(function(){return B(unCStr("Iconv.close"));}),_17V=function(_17W,_17X,_){var _17Y=newByteArr(B(_IP(_17W,0))+1|0),_17Z=_17Y,_180=_17Z,_181=_180,_182=_181,_183=B((function(_184,_185,_){while(1){var _186=E(_184);if(!_186[0]){var _=writeOffAddr("i8",1,_182,_185,0);return _c;}else{var _=writeOffAddr("i8",1,_182,_185,E(_186[1])[1]&255);_184=_186[2];var _187=_185+1|0;_185=_187;continue;}}})(_17W,0,_)),_188=_183,_189=B(A(_17X,[[0,_182],_])),_18a=_189,_=0;return _18a;},_18b=function(_18c,_18d,_){return new F(function(){return _17V(_18c,_18d,_);});},_18e=function(_18f,_18g,_18h,_18i){return new F(function(){return _18b(_18f,function(_18j){return new F(function(){return _18b(_18g,function(_18k,_){var _18l=hs_iconv_open(E(_18k)[1],E(_18j)[1]),_18m=_18l,_18n=E(_18m);if(_18n==(-1)){var _18o=__hscore_get_errno(),_18p=_18o;return new F(function(){return _16r(B(_17A(_17T,_18p,_5h,_5h)),_);});}else{return [0,new T(function(){return B(A(_18i,[[0,_18n]]));}),_18h,function(_){var _18q=hs_iconv_close(_18n),_18r=_18q;if(E(_18r)==(-1)){var _18s=__hscore_get_errno(),_18t=_18s;return new F(function(){return _16r(B(_17A(_17U,_18t,_5h,_5h)),_);});}else{return _c;}},_17S,_17Q];}});});});});},_18u=function(_16s,_){return new F(function(){return _16p(_16s,_);});},_18v=12,_18w=new T(function(){return B(unCStr("invalid byte sequence"));}),_18x=new T(function(){return B(unCStr("recoverDecode"));}),_18y=[0,_5h,_18v,_18x,_18w,_5h,_5h],_18z=function(_18A,_18B,_18C,_18D,_18E,_18F,_18G,_18H,_18I,_18J,_18K,_18L,_18M,_){switch(E(_18A)){case 0:return new F(function(){return _18u(_18y,_);});break;case 1:return [0,[0,_18B,_18C,_18D,_18E,_18F+1|0,_18G],[0,_18H,_18I,_18J,_18K,_18L,_18M]];case 2:var _=writeOffAddr("w32",4,_18H,_18M,65533),_=0;return [0,[0,_18B,_18C,_18D,_18E,_18F+1|0,_18G],[0,_18H,_18I,_18J,_18K,_18L,_18M+1|0]];default:var _18N=readOffAddr("w8",1,plusAddr(_18B,_18F),0),_18O=_18N,_=0;if(_18O>=128){var _18P=56320+(_18O&4294967295)|0;if(_18P>>>0>1114111){return new F(function(){return _s0(_18P);});}else{var _=writeOffAddr("w32",4,_18H,_18M,_18P),_=0;return [0,[0,_18B,_18C,_18D,_18E,_18F+1|0,_18G],[0,_18H,_18I,_18J,_18K,_18L,_18M+1|0]];}}else{var _18Q=_18O&4294967295;if(_18Q>>>0>1114111){return new F(function(){return _s0(_18Q);});}else{var _=writeOffAddr("w32",4,_18H,_18M,_18Q),_=0;return [0,[0,_18B,_18C,_18D,_18E,_18F+1|0,_18G],[0,_18H,_18I,_18J,_18K,_18L,_18M+1|0]];}}}},_18R=function(_18S,_18T,_18U,_){var _18V=E(_18T),_18W=E(_18U);return new F(function(){return _18z(_18S,_18V[1],_18V[2],_18V[3],_18V[4],_18V[5],_18V[6],_18W[1],_18W[2],_18W[3],_18W[4],_18W[5],_18W[6],_);});},_18X=new T(function(){return B(unCStr("recoverEncode"));}),_18Y=new T(function(){return B(unCStr("invalid character"));}),_18Z=[0,_5h,_18v,_18X,_18Y,_5h,_5h],_190=function(_){return new F(function(){return _18u(_18Z,_);});},_191=function(_192,_193,_194,_195,_196,_197,_198,_199,_19a,_19b,_19c,_19d,_19e,_){var _19f=readOffAddr("w32",4,_193,_197),_19g=_19f,_=0;switch(E(_192)){case 0:return new F(function(){return _190(_);});break;case 1:return [0,[0,_193,_194,_195,_196,_197+1|0,_198],[0,_199,_19a,_19b,_19c,_19d,_19e]];case 2:if(E(_19g)==63){return [0,[0,_193,_194,_195,_196,_197+1|0,_198],[0,_199,_19a,_19b,_19c,_19d,_19e]];}else{var _=writeOffAddr("w32",4,_193,_197,63),_=0;return [0,[0,_193,_194,_195,_196,_197,_198],[0,_199,_19a,_19b,_19c,_19d,_19e]];}break;default:var _19h=_19g;if(56448>_19h){return new F(function(){return _190(_);});}else{if(_19h>=56576){return new F(function(){return _190(_);});}else{var _=writeOffAddr("w8",1,plusAddr(_199,_19e),0,_19h>>>0&255),_=0;return [0,[0,_193,_194,_195,_196,_197+1|0,_198],[0,_199,_19a,_19b,_19c,_19d,_19e+1|0]];}}}},_19i=function(_19j,_19k,_19l,_){var _19m=E(_19k),_19n=E(_19l);return new F(function(){return _191(_19j,_19m[1],_19m[2],_19m[3],_19m[4],_19m[5],_19m[6],_19n[1],_19n[2],_19n[3],_19n[4],_19n[5],_19n[6],_);});},_19o=function(_19p,_19q,_){return [0,_19q,new T(function(){var _19r=new T(function(){var _19s=B(_eC(_17O,_19q));return [0,_19s[1],_19s[2]];});return B(_18e(new T(function(){return E(E(_19r)[1]);}),new T(function(){return B(_2J(_16k,new T(function(){return E(E(_19r)[2]);},1)));}),function(_19t,_19u,_){return new F(function(){return _18R(_19p,_19t,_19u,_);});},_17I));}),new T(function(){return B(_18e(_16k,_19q,function(_19t,_19u,_){return new F(function(){return _19i(_19p,_19t,_19u,_);});},_17C));})];},_19v=2,_19w=function(_19x,_19y,_19z,_19A,_19B,_19C,_19D,_19E,_19F,_19G,_19H,_19I,_){var _19J=[0,_19x,_19y,_19z,_19A,0,0],_19K=function(_19L,_19M,_){while(1){var _19N=(function(_19O,_19P,_){if(_19O<_19C){if((_19G-_19P|0)>=2){var _19Q=readOffAddr("w32",4,_19x,_19O),_19R=_19Q,_=0,_19S=_19R;if(_19S>=65536){if((_19G-_19P|0)>=4){var _19T=_19S-65536|0,_=writeOffAddr("w8",1,plusAddr(_19D,_19P),0,((_19T>>18)+216|0)>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_19D,_19P+1|0),0,_19T>>10>>>0&255),_=0,_19U=(_19T>>>0&1023>>>0)>>>0&4294967295,_=writeOffAddr("w8",1,plusAddr(_19D,_19P+2|0),0,((_19U>>8)+220|0)>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_19D,_19P+3|0),0,_19U>>>0&255),_=0,_19V=_19O+1|0,_19W=_19P+4|0;_19L=_19V;_19M=_19W;return null;}else{return [0,_16m,new T(function(){return _19O!=_19C?[0,_19x,_19y,_19z,_19A,_19O,_19C]:E(_19J);}),[0,_19D,_19E,_19F,_19G,_19H,_19P]];}}else{var _19X=function(_19Y){if(56320>_19S){var _=writeOffAddr("w8",1,plusAddr(_19D,_19P),0,_19S>>8>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_19D,_19P+1|0),0,_19S>>>0&255),_=0;return new F(function(){return _19K(_19O+1|0,_19P+2|0,_);});}else{if(_19S>57343){var _=writeOffAddr("w8",1,plusAddr(_19D,_19P),0,_19S>>8>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_19D,_19P+1|0),0,_19S>>>0&255),_=0;return new F(function(){return _19K(_19O+1|0,_19P+2|0,_);});}else{return [0,_19v,new T(function(){return _19O!=_19C?[0,_19x,_19y,_19z,_19A,_19O,_19C]:E(_19J);}),[0,_19D,_19E,_19F,_19G,_19H,_19P]];}}};if(55296>_19S){return new F(function(){return _19X(_);});}else{return _19S>56319?B(_19X(_)):[0,_19v,new T(function(){return _19O!=_19C?[0,_19x,_19y,_19z,_19A,_19O,_19C]:E(_19J);}),[0,_19D,_19E,_19F,_19G,_19H,_19P]];}}}else{return [0,_16m,new T(function(){return _19O!=_19C?[0,_19x,_19y,_19z,_19A,_19O,_19C]:E(_19J);}),[0,_19D,_19E,_19F,_19G,_19H,_19P]];}}else{return [0,_16l,new T(function(){return _19O!=_19C?[0,_19x,_19y,_19z,_19A,_19O,_19C]:E(_19J);}),[0,_19D,_19E,_19F,_19G,_19H,_19P]];}})(_19L,_19M,_);if(_19N!=null){return _19N;}}};return new F(function(){return _19K(_19B,_19I,_);});},_19Z=function(_1a0,_1a1,_1a2,_1a3,_1a4,_1a5,_1a6,_1a7,_){var _1a8=rMV(_1a0),_1a9=_1a8;if(!E(_1a9)){if((_1a5-_1a7|0)>=2){var _=wMV(_1a0,_gw),_=writeOffAddr("w8",1,plusAddr(_1a2,_1a7),0,254),_=0,_=writeOffAddr("w8",1,plusAddr(_1a2,_1a7+1|0),0,255),_=0,_1aa=E(_1a1);return new F(function(){return _19w(_1aa[1],_1aa[2],_1aa[3],_1aa[4],_1aa[5],_1aa[6],_1a2,_1a3,_1a4,_1a5,_1a6,_1a7+2|0,_);});}else{return [0,_16m,_1a1,[0,_1a2,_1a3,_1a4,_1a5,_1a6,_1a7]];}}else{var _1ab=E(_1a1);return new F(function(){return _19w(_1ab[1],_1ab[2],_1ab[3],_1ab[4],_1ab[5],_1ab[6],_1a2,_1a3,_1a4,_1a5,_1a6,_1a7,_);});}},_1ac=function(_1ad,_1ae,_1af,_1ag,_1ah,_1ai,_1aj,_1ak,_1al,_1am,_1an,_1ao,_){var _1ap=[0,_1ad,_1ae,_1af,_1ag,0,0];return new F(function(){return (function(_1aq,_1ar,_){while(1){var _1as=(function(_1at,_1au,_){if(_1au<_1am){if(_1at<_1ai){if((_1at+1|0)!=_1ai){var _1av=readOffAddr("w8",1,plusAddr(_1ad,_1at),0),_1aw=_1av,_=0,_1ax=readOffAddr("w8",1,plusAddr(_1ad,_1at+1|0),0),_1ay=_1ax,_=0,_1az=(_1aw<<8>>>0&65535)+_1ay>>>0&65535;if(_1az>=55296){if(_1az<=57343){if((_1ai-_1at|0)>=4){var _1aA=readOffAddr("w8",1,plusAddr(_1ad,_1at+2|0),0),_1aB=_1aA,_=0,_1aC=readOffAddr("w8",1,plusAddr(_1ad,_1at+3|0),0),_1aD=_1aC,_=0;if(_1az<55296){return [0,_19v,new T(function(){return _1at!=_1ai?[0,_1ad,_1ae,_1af,_1ag,_1at,_1ai]:E(_1ap);}),[0,_1aj,_1ak,_1al,_1am,_1an,_1au]];}else{if(_1az>56319){return [0,_19v,new T(function(){return _1at!=_1ai?[0,_1ad,_1ae,_1af,_1ag,_1at,_1ai]:E(_1ap);}),[0,_1aj,_1ak,_1al,_1am,_1an,_1au]];}else{var _1aE=(_1aB<<8>>>0&65535)+_1aD>>>0&65535;if(_1aE<56320){return [0,_19v,new T(function(){return _1at!=_1ai?[0,_1ad,_1ae,_1af,_1ag,_1at,_1ai]:E(_1ap);}),[0,_1aj,_1ak,_1al,_1am,_1an,_1au]];}else{if(_1aE>57343){return [0,_19v,new T(function(){return _1at!=_1ai?[0,_1ad,_1ae,_1af,_1ag,_1at,_1ai]:E(_1ap);}),[0,_1aj,_1ak,_1al,_1am,_1an,_1au]];}else{var _=writeOffAddr("w32",4,_1aj,_1au,((((_1az&4294967295)-55296|0)<<10)+((_1aE&4294967295)-56320|0)|0)+65536|0),_=0,_1aF=_1at+4|0,_1aG=_1au+1|0;_1aq=_1aF;_1ar=_1aG;return null;}}}}}else{return [0,_16l,new T(function(){return _1at!=_1ai?[0,_1ad,_1ae,_1af,_1ag,_1at,_1ai]:E(_1ap);}),[0,_1aj,_1ak,_1al,_1am,_1an,_1au]];}}else{var _=writeOffAddr("w32",4,_1aj,_1au,_1az&4294967295),_=0,_1aF=_1at+2|0,_1aG=_1au+1|0;_1aq=_1aF;_1ar=_1aG;return null;}}else{var _=writeOffAddr("w32",4,_1aj,_1au,_1az&4294967295),_=0,_1aF=_1at+2|0,_1aG=_1au+1|0;_1aq=_1aF;_1ar=_1aG;return null;}}else{return [0,_16l,new T(function(){return _1at!=_1ai?[0,_1ad,_1ae,_1af,_1ag,_1at,_1ai]:E(_1ap);}),[0,_1aj,_1ak,_1al,_1am,_1an,_1au]];}}else{return [0,_16l,new T(function(){return _1at!=_1ai?[0,_1ad,_1ae,_1af,_1ag,_1at,_1ai]:E(_1ap);}),[0,_1aj,_1ak,_1al,_1am,_1an,_1au]];}}else{return [0,_16m,new T(function(){return _1at!=_1ai?[0,_1ad,_1ae,_1af,_1ag,_1at,_1ai]:E(_1ap);}),[0,_1aj,_1ak,_1al,_1am,_1an,_1au]];}})(_1aq,_1ar,_);if(_1as!=null){return _1as;}}})(_1ah,_1ao,_);});},_1aH=function(_1aI,_1aJ,_1aK,_1aL,_1aM,_1aN,_1aO,_1aP,_1aQ,_1aR,_1aS,_1aT,_){var _1aU=[0,_1aI,_1aJ,_1aK,_1aL,0,0];return new F(function(){return (function(_1aV,_1aW,_){while(1){var _1aX=(function(_1aY,_1aZ,_){if(_1aZ<_1aR){if(_1aY<_1aN){if((_1aY+1|0)!=_1aN){var _1b0=readOffAddr("w8",1,plusAddr(_1aI,_1aY),0),_1b1=_1b0,_=0,_1b2=readOffAddr("w8",1,plusAddr(_1aI,_1aY+1|0),0),_1b3=_1b2,_=0,_1b4=(_1b3<<8>>>0&65535)+_1b1>>>0&65535;if(_1b4>=55296){if(_1b4<=57343){if((_1aN-_1aY|0)>=4){var _1b5=readOffAddr("w8",1,plusAddr(_1aI,_1aY+2|0),0),_1b6=_1b5,_=0,_1b7=readOffAddr("w8",1,plusAddr(_1aI,_1aY+3|0),0),_1b8=_1b7,_=0;if(_1b4<55296){return [0,_19v,new T(function(){return _1aY!=_1aN?[0,_1aI,_1aJ,_1aK,_1aL,_1aY,_1aN]:E(_1aU);}),[0,_1aO,_1aP,_1aQ,_1aR,_1aS,_1aZ]];}else{if(_1b4>56319){return [0,_19v,new T(function(){return _1aY!=_1aN?[0,_1aI,_1aJ,_1aK,_1aL,_1aY,_1aN]:E(_1aU);}),[0,_1aO,_1aP,_1aQ,_1aR,_1aS,_1aZ]];}else{var _1b9=(_1b8<<8>>>0&65535)+_1b6>>>0&65535;if(_1b9<56320){return [0,_19v,new T(function(){return _1aY!=_1aN?[0,_1aI,_1aJ,_1aK,_1aL,_1aY,_1aN]:E(_1aU);}),[0,_1aO,_1aP,_1aQ,_1aR,_1aS,_1aZ]];}else{if(_1b9>57343){return [0,_19v,new T(function(){return _1aY!=_1aN?[0,_1aI,_1aJ,_1aK,_1aL,_1aY,_1aN]:E(_1aU);}),[0,_1aO,_1aP,_1aQ,_1aR,_1aS,_1aZ]];}else{var _=writeOffAddr("w32",4,_1aO,_1aZ,((((_1b4&4294967295)-55296|0)<<10)+((_1b9&4294967295)-56320|0)|0)+65536|0),_=0,_1ba=_1aY+4|0,_1bb=_1aZ+1|0;_1aV=_1ba;_1aW=_1bb;return null;}}}}}else{return [0,_16l,new T(function(){return _1aY!=_1aN?[0,_1aI,_1aJ,_1aK,_1aL,_1aY,_1aN]:E(_1aU);}),[0,_1aO,_1aP,_1aQ,_1aR,_1aS,_1aZ]];}}else{var _=writeOffAddr("w32",4,_1aO,_1aZ,_1b4&4294967295),_=0,_1ba=_1aY+2|0,_1bb=_1aZ+1|0;_1aV=_1ba;_1aW=_1bb;return null;}}else{var _=writeOffAddr("w32",4,_1aO,_1aZ,_1b4&4294967295),_=0,_1ba=_1aY+2|0,_1bb=_1aZ+1|0;_1aV=_1ba;_1aW=_1bb;return null;}}else{return [0,_16l,new T(function(){return _1aY!=_1aN?[0,_1aI,_1aJ,_1aK,_1aL,_1aY,_1aN]:E(_1aU);}),[0,_1aO,_1aP,_1aQ,_1aR,_1aS,_1aZ]];}}else{return [0,_16l,new T(function(){return _1aY!=_1aN?[0,_1aI,_1aJ,_1aK,_1aL,_1aY,_1aN]:E(_1aU);}),[0,_1aO,_1aP,_1aQ,_1aR,_1aS,_1aZ]];}}else{return [0,_16m,new T(function(){return _1aY!=_1aN?[0,_1aI,_1aJ,_1aK,_1aL,_1aY,_1aN]:E(_1aU);}),[0,_1aO,_1aP,_1aQ,_1aR,_1aS,_1aZ]];}})(_1aV,_1aW,_);if(_1aX!=null){return _1aX;}}})(_1aM,_1aT,_);});},_1bc=function(_1bd,_1be,_){var _1bf=E(_1bd),_1bg=E(_1be);return new F(function(){return _1ac(_1bf[1],_1bf[2],_1bf[3],_1bf[4],_1bf[5],_1bf[6],_1bg[1],_1bg[2],_1bg[3],_1bg[4],_1bg[5],_1bg[6],_);});},_1bh=[1,_1bc],_1bi=function(_1bj,_1bk,_){var _1bl=E(_1bj),_1bm=E(_1bk);return new F(function(){return _1aH(_1bl[1],_1bl[2],_1bl[3],_1bl[4],_1bl[5],_1bl[6],_1bm[1],_1bm[2],_1bm[3],_1bm[4],_1bm[5],_1bm[6],_);});},_1bn=[1,_1bi],_1bo=function(_1bp,_1bq,_1br,_1bs,_1bt,_1bu,_1bv,_1bw,_){var _1bx=rMV(_1bp),_1by=_1bx,_1bz=E(_1by);if(!_1bz[0]){if((_1bv-_1bu|0)>=2){var _1bA=readOffAddr("w8",1,plusAddr(_1bq,_1bu),0),_1bB=_1bA,_=0,_1bC=readOffAddr("w8",1,plusAddr(_1bq,_1bu+1|0),0),_1bD=_1bC,_=0,_1bE=function(_1bF){if(E(_1bB)==255){if(E(_1bD)==254){var _=wMV(_1bp,_1bn),_1bG=E(_1bw);return new F(function(){return _1aH(_1bq,_1br,_1bs,_1bt,_1bu+2|0,_1bv,_1bG[1],_1bG[2],_1bG[3],_1bG[4],_1bG[5],_1bG[6],_);});}else{var _=wMV(_1bp,_1bh),_1bH=E(_1bw);return new F(function(){return _1ac(_1bq,_1br,_1bs,_1bt,_1bu,_1bv,_1bH[1],_1bH[2],_1bH[3],_1bH[4],_1bH[5],_1bH[6],_);});}}else{var _=wMV(_1bp,_1bh),_1bI=E(_1bw);return new F(function(){return _1ac(_1bq,_1br,_1bs,_1bt,_1bu,_1bv,_1bI[1],_1bI[2],_1bI[3],_1bI[4],_1bI[5],_1bI[6],_);});}};if(E(_1bB)==254){if(E(_1bD)==255){var _=wMV(_1bp,_1bh),_1bJ=E(_1bw);return new F(function(){return _1ac(_1bq,_1br,_1bs,_1bt,_1bu+2|0,_1bv,_1bJ[1],_1bJ[2],_1bJ[3],_1bJ[4],_1bJ[5],_1bJ[6],_);});}else{return new F(function(){return _1bE(_);});}}else{return new F(function(){return _1bE(_);});}}else{return [0,_16l,[0,_1bq,_1br,_1bs,_1bt,_1bu,_1bv],_1bw];}}else{return new F(function(){return A(_1bz[1],[[0,_1bq,_1br,_1bs,_1bt,_1bu,_1bv],_1bw,_]);});}},_1bK=function(_){return _c;},_1bL=new T(function(){return B(unCStr("UTF-16"));}),_1bM=function(_1bN){return [0,_1bL,function(_){var _1bO=nMV(_5h),_1bP=_1bO;return [0,function(_1bQ,_1bR,_){var _1bS=E(_1bQ);return new F(function(){return _1bo(_1bP,_1bS[1],_1bS[2],_1bS[3],_1bS[4],_1bS[5],_1bS[6],_1bR,_);});},function(_1bT,_1bU,_){return new F(function(){return _18R(_1bN,_1bT,_1bU,_);});},_1bK,function(_){return new F(function(){return rMV(_1bP);});},function(_1bV,_){var _=wMV(_1bP,_1bV);return _c;}];},function(_){var _1bW=nMV(_5E),_1bX=_1bW;return [0,function(_1bY,_1bZ,_){var _1c0=E(_1bZ);return new F(function(){return _19Z(_1bX,_1bY,_1c0[1],_1c0[2],_1c0[3],_1c0[4],_1c0[5],_1c0[6],_);});},function(_1bT,_1bU,_){return new F(function(){return _19i(_1bN,_1bT,_1bU,_);});},_1bK,function(_){return new F(function(){return rMV(_1bX);});},function(_1c1,_){var _=wMV(_1bX,_1c1);return _c;}];}];},_1c2=function(_1c3,_1c4,_){var _1c5=E(_1c3),_1c6=E(_1c4);return new F(function(){return _19w(_1c5[1],_1c5[2],_1c5[3],_1c5[4],_1c5[5],_1c5[6],_1c6[1],_1c6[2],_1c6[3],_1c6[4],_1c6[5],_1c6[6],_);});},_1c7=function(_1c8,_){return _c;},_1c9=new T(function(){return B(unCStr("UTF-16BE"));}),_1ca=function(_1cb){return [0,_1c9,function(_){return [0,_1bc,function(_1bT,_1bU,_){return new F(function(){return _18R(_1cb,_1bT,_1bU,_);});},_1bK,_1bK,_1c7];},function(_){return [0,_1c2,function(_1bT,_1bU,_){return new F(function(){return _19i(_1cb,_1bT,_1bU,_);});},_1bK,_1bK,_1c7];}];},_1cc=function(_1cd,_1ce,_1cf,_1cg,_1ch,_1ci,_1cj,_1ck,_1cl,_1cm,_1cn,_1co,_){var _1cp=[0,_1cd,_1ce,_1cf,_1cg,0,0],_1cq=function(_1cr,_1cs,_){while(1){var _1ct=(function(_1cu,_1cv,_){if(_1cu<_1ci){if((_1cm-_1cv|0)>=2){var _1cw=readOffAddr("w32",4,_1cd,_1cu),_1cx=_1cw,_=0,_1cy=_1cx;if(_1cy>=65536){if((_1cm-_1cv|0)>=4){var _1cz=_1cy-65536|0,_=writeOffAddr("w8",1,plusAddr(_1cj,_1cv),0,_1cz>>10>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_1cj,_1cv+1|0),0,((_1cz>>18)+216|0)>>>0&255),_=0,_1cA=(_1cz>>>0&1023>>>0)>>>0&4294967295,_=writeOffAddr("w8",1,plusAddr(_1cj,_1cv+2|0),0,_1cA>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_1cj,_1cv+3|0),0,((_1cA>>8)+220|0)>>>0&255),_=0,_1cB=_1cu+1|0,_1cC=_1cv+4|0;_1cr=_1cB;_1cs=_1cC;return null;}else{return [0,_16m,new T(function(){return _1cu!=_1ci?[0,_1cd,_1ce,_1cf,_1cg,_1cu,_1ci]:E(_1cp);}),[0,_1cj,_1ck,_1cl,_1cm,_1cn,_1cv]];}}else{var _1cD=function(_1cE){if(56320>_1cy){var _=writeOffAddr("w8",1,plusAddr(_1cj,_1cv),0,_1cy>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_1cj,_1cv+1|0),0,_1cy>>8>>>0&255),_=0;return new F(function(){return _1cq(_1cu+1|0,_1cv+2|0,_);});}else{if(_1cy>57343){var _=writeOffAddr("w8",1,plusAddr(_1cj,_1cv),0,_1cy>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_1cj,_1cv+1|0),0,_1cy>>8>>>0&255),_=0;return new F(function(){return _1cq(_1cu+1|0,_1cv+2|0,_);});}else{return [0,_19v,new T(function(){return _1cu!=_1ci?[0,_1cd,_1ce,_1cf,_1cg,_1cu,_1ci]:E(_1cp);}),[0,_1cj,_1ck,_1cl,_1cm,_1cn,_1cv]];}}};if(55296>_1cy){return new F(function(){return _1cD(_);});}else{return _1cy>56319?B(_1cD(_)):[0,_19v,new T(function(){return _1cu!=_1ci?[0,_1cd,_1ce,_1cf,_1cg,_1cu,_1ci]:E(_1cp);}),[0,_1cj,_1ck,_1cl,_1cm,_1cn,_1cv]];}}}else{return [0,_16m,new T(function(){return _1cu!=_1ci?[0,_1cd,_1ce,_1cf,_1cg,_1cu,_1ci]:E(_1cp);}),[0,_1cj,_1ck,_1cl,_1cm,_1cn,_1cv]];}}else{return [0,_16l,new T(function(){return _1cu!=_1ci?[0,_1cd,_1ce,_1cf,_1cg,_1cu,_1ci]:E(_1cp);}),[0,_1cj,_1ck,_1cl,_1cm,_1cn,_1cv]];}})(_1cr,_1cs,_);if(_1ct!=null){return _1ct;}}};return new F(function(){return _1cq(_1ch,_1co,_);});},_1cF=function(_1cG,_1cH,_){var _1cI=E(_1cG),_1cJ=E(_1cH);return new F(function(){return _1cc(_1cI[1],_1cI[2],_1cI[3],_1cI[4],_1cI[5],_1cI[6],_1cJ[1],_1cJ[2],_1cJ[3],_1cJ[4],_1cJ[5],_1cJ[6],_);});},_1cK=new T(function(){return B(unCStr("UTF16-LE"));}),_1cL=function(_1cM){return [0,_1cK,function(_){return [0,_1bi,function(_1bT,_1bU,_){return new F(function(){return _18R(_1cM,_1bT,_1bU,_);});},_1bK,_1bK,_1c7];},function(_){return [0,_1cF,function(_1bT,_1bU,_){return new F(function(){return _19i(_1cM,_1bT,_1bU,_);});},_1bK,_1bK,_1c7];}];},_1cN=function(_1cO,_1cP,_1cQ,_1cR,_1cS,_1cT,_1cU,_1cV,_1cW,_1cX,_1cY,_1cZ,_){var _1d0=[0,_1cO,_1cP,_1cQ,_1cR,0,0],_1d1=function(_1d2,_1d3,_){if(_1d2<_1cT){if((_1cX-_1d3|0)>=4){var _1d4=readOffAddr("w32",4,_1cO,_1d2),_1d5=_1d4,_=0,_1d6=_1d5,_1d7=function(_1d8){if(56320>_1d6){var _=writeOffAddr("w8",1,plusAddr(_1cU,_1d3),0,_1d6>>24>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_1cU,_1d3+1|0),0,_1d6>>16>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_1cU,_1d3+2|0),0,_1d6>>8>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_1cU,_1d3+3|0),0,_1d6>>>0&255),_=0;return new F(function(){return _1d1(_1d2+1|0,_1d3+4|0,_);});}else{if(_1d6>57343){var _=writeOffAddr("w8",1,plusAddr(_1cU,_1d3),0,_1d6>>24>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_1cU,_1d3+1|0),0,_1d6>>16>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_1cU,_1d3+2|0),0,_1d6>>8>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_1cU,_1d3+3|0),0,_1d6>>>0&255),_=0;return new F(function(){return _1d1(_1d2+1|0,_1d3+4|0,_);});}else{return [0,_19v,new T(function(){return _1d2!=_1cT?[0,_1cO,_1cP,_1cQ,_1cR,_1d2,_1cT]:E(_1d0);}),[0,_1cU,_1cV,_1cW,_1cX,_1cY,_1d3]];}}};if(55296>_1d6){return new F(function(){return _1d7(_);});}else{return _1d6>56319?B(_1d7(_)):[0,_19v,new T(function(){return _1d2!=_1cT?[0,_1cO,_1cP,_1cQ,_1cR,_1d2,_1cT]:E(_1d0);}),[0,_1cU,_1cV,_1cW,_1cX,_1cY,_1d3]];}}else{return [0,_16m,new T(function(){return _1d2!=_1cT?[0,_1cO,_1cP,_1cQ,_1cR,_1d2,_1cT]:E(_1d0);}),[0,_1cU,_1cV,_1cW,_1cX,_1cY,_1d3]];}}else{return [0,_16l,new T(function(){return _1d2!=_1cT?[0,_1cO,_1cP,_1cQ,_1cR,_1d2,_1cT]:E(_1d0);}),[0,_1cU,_1cV,_1cW,_1cX,_1cY,_1d3]];}};return new F(function(){return _1d1(_1cS,_1cZ,_);});},_1d9=function(_1da,_1db,_1dc,_1dd,_1de,_1df,_1dg,_1dh,_){var _1di=rMV(_1da),_1dj=_1di;if(!E(_1dj)){if((_1df-_1dh|0)>=4){var _=wMV(_1da,_gw),_=writeOffAddr("w8",1,plusAddr(_1dc,_1dh),0,0),_=0,_=writeOffAddr("w8",1,plusAddr(_1dc,_1dh+1|0),0,0),_=0,_=writeOffAddr("w8",1,plusAddr(_1dc,_1dh+2|0),0,254),_=0,_=writeOffAddr("w8",1,plusAddr(_1dc,_1dh+3|0),0,255),_=0,_1dk=E(_1db);return new F(function(){return _1cN(_1dk[1],_1dk[2],_1dk[3],_1dk[4],_1dk[5],_1dk[6],_1dc,_1dd,_1de,_1df,_1dg,_1dh+4|0,_);});}else{return [0,_16m,_1db,[0,_1dc,_1dd,_1de,_1df,_1dg,_1dh]];}}else{var _1dl=E(_1db);return new F(function(){return _1cN(_1dl[1],_1dl[2],_1dl[3],_1dl[4],_1dl[5],_1dl[6],_1dc,_1dd,_1de,_1df,_1dg,_1dh,_);});}},_1dm=function(_1dn,_1do,_1dp,_1dq,_1dr,_1ds,_1dt,_1du,_1dv,_1dw,_1dx,_1dy,_){var _1dz=[0,_1dn,_1do,_1dp,_1dq,0,0],_1dA=function(_1dB,_1dC,_){while(1){var _1dD=(function(_1dE,_1dF,_){if(_1dF<_1dw){if((_1ds-_1dE|0)>=4){var _1dG=readOffAddr("w8",1,plusAddr(_1dn,_1dE),0),_1dH=_1dG,_=0,_1dI=readOffAddr("w8",1,plusAddr(_1dn,_1dE+1|0),0),_1dJ=_1dI,_=0,_1dK=readOffAddr("w8",1,plusAddr(_1dn,_1dE+2|0),0),_1dL=_1dK,_=0,_1dM=readOffAddr("w8",1,plusAddr(_1dn,_1dE+3|0),0),_1dN=_1dM,_=0,_1dO=((((_1dH&4294967295)<<24)+((_1dJ&4294967295)<<16)|0)+((_1dL&4294967295)<<8)|0)+(_1dN&4294967295)|0,_1dP=_1dO,_1dQ=function(_1dR){if(_1dP<=57343){return [0,_19v,new T(function(){return _1dE!=_1ds?[0,_1dn,_1do,_1dp,_1dq,_1dE,_1ds]:E(_1dz);}),[0,_1dt,_1du,_1dv,_1dw,_1dx,_1dF]];}else{if(_1dP>1114111){return [0,_19v,new T(function(){return _1dE!=_1ds?[0,_1dn,_1do,_1dp,_1dq,_1dE,_1ds]:E(_1dz);}),[0,_1dt,_1du,_1dv,_1dw,_1dx,_1dF]];}else{var _=writeOffAddr("w32",4,_1dt,_1dF,_1dO),_=0;return new F(function(){return _1dA(_1dE+4|0,_1dF+1|0,_);});}}};if(_1dP<0){return new F(function(){return _1dQ(_);});}else{if(_1dP>=55296){return new F(function(){return _1dQ(_);});}else{var _=writeOffAddr("w32",4,_1dt,_1dF,_1dO),_=0,_1dS=_1dE+4|0,_1dT=_1dF+1|0;_1dB=_1dS;_1dC=_1dT;return null;}}}else{return [0,_16l,new T(function(){return _1dE!=_1ds?[0,_1dn,_1do,_1dp,_1dq,_1dE,_1ds]:E(_1dz);}),[0,_1dt,_1du,_1dv,_1dw,_1dx,_1dF]];}}else{return [0,_16m,new T(function(){return _1dE!=_1ds?[0,_1dn,_1do,_1dp,_1dq,_1dE,_1ds]:E(_1dz);}),[0,_1dt,_1du,_1dv,_1dw,_1dx,_1dF]];}})(_1dB,_1dC,_);if(_1dD!=null){return _1dD;}}};return new F(function(){return _1dA(_1dr,_1dy,_);});},_1dU=function(_1dV,_1dW,_1dX,_1dY,_1dZ,_1e0,_1e1,_1e2,_1e3,_1e4,_1e5,_1e6,_){var _1e7=[0,_1dV,_1dW,_1dX,_1dY,0,0],_1e8=function(_1e9,_1ea,_){while(1){var _1eb=(function(_1ec,_1ed,_){if(_1ed<_1e4){if((_1e0-_1ec|0)>=4){var _1ee=readOffAddr("w8",1,plusAddr(_1dV,_1ec),0),_1ef=_1ee,_=0,_1eg=readOffAddr("w8",1,plusAddr(_1dV,_1ec+1|0),0),_1eh=_1eg,_=0,_1ei=readOffAddr("w8",1,plusAddr(_1dV,_1ec+2|0),0),_1ej=_1ei,_=0,_1ek=readOffAddr("w8",1,plusAddr(_1dV,_1ec+3|0),0),_1el=_1ek,_=0,_1em=((((_1el&4294967295)<<24)+((_1ej&4294967295)<<16)|0)+((_1eh&4294967295)<<8)|0)+(_1ef&4294967295)|0,_1en=_1em,_1eo=function(_1ep){if(_1en<=57343){return [0,_19v,new T(function(){return _1ec!=_1e0?[0,_1dV,_1dW,_1dX,_1dY,_1ec,_1e0]:E(_1e7);}),[0,_1e1,_1e2,_1e3,_1e4,_1e5,_1ed]];}else{if(_1en>1114111){return [0,_19v,new T(function(){return _1ec!=_1e0?[0,_1dV,_1dW,_1dX,_1dY,_1ec,_1e0]:E(_1e7);}),[0,_1e1,_1e2,_1e3,_1e4,_1e5,_1ed]];}else{var _=writeOffAddr("w32",4,_1e1,_1ed,_1em),_=0;return new F(function(){return _1e8(_1ec+4|0,_1ed+1|0,_);});}}};if(_1en<0){return new F(function(){return _1eo(_);});}else{if(_1en>=55296){return new F(function(){return _1eo(_);});}else{var _=writeOffAddr("w32",4,_1e1,_1ed,_1em),_=0,_1eq=_1ec+4|0,_1er=_1ed+1|0;_1e9=_1eq;_1ea=_1er;return null;}}}else{return [0,_16l,new T(function(){return _1ec!=_1e0?[0,_1dV,_1dW,_1dX,_1dY,_1ec,_1e0]:E(_1e7);}),[0,_1e1,_1e2,_1e3,_1e4,_1e5,_1ed]];}}else{return [0,_16m,new T(function(){return _1ec!=_1e0?[0,_1dV,_1dW,_1dX,_1dY,_1ec,_1e0]:E(_1e7);}),[0,_1e1,_1e2,_1e3,_1e4,_1e5,_1ed]];}})(_1e9,_1ea,_);if(_1eb!=null){return _1eb;}}};return new F(function(){return _1e8(_1dZ,_1e6,_);});},_1es=function(_1et,_1eu,_){var _1ev=E(_1et),_1ew=E(_1eu);return new F(function(){return _1dm(_1ev[1],_1ev[2],_1ev[3],_1ev[4],_1ev[5],_1ev[6],_1ew[1],_1ew[2],_1ew[3],_1ew[4],_1ew[5],_1ew[6],_);});},_1ex=[1,_1es],_1ey=function(_1ez,_1eA,_){var _1eB=E(_1ez),_1eC=E(_1eA);return new F(function(){return _1dU(_1eB[1],_1eB[2],_1eB[3],_1eB[4],_1eB[5],_1eB[6],_1eC[1],_1eC[2],_1eC[3],_1eC[4],_1eC[5],_1eC[6],_);});},_1eD=[1,_1ey],_1eE=function(_1eF,_1eG,_1eH,_1eI,_1eJ,_1eK,_1eL,_1eM,_){var _1eN=rMV(_1eF),_1eO=_1eN,_1eP=E(_1eO);if(!_1eP[0]){if((_1eL-_1eK|0)>=4){var _1eQ=readOffAddr("w8",1,plusAddr(_1eG,_1eK),0),_1eR=_1eQ,_=0,_1eS=readOffAddr("w8",1,plusAddr(_1eG,_1eK+1|0),0),_1eT=_1eS,_=0,_1eU=readOffAddr("w8",1,plusAddr(_1eG,_1eK+2|0),0),_1eV=_1eU,_=0,_1eW=readOffAddr("w8",1,plusAddr(_1eG,_1eK+3|0),0),_1eX=_1eW,_=0,_1eY=function(_1eZ){if(E(_1eR)==255){if(E(_1eT)==254){if(!E(_1eV)){if(!E(_1eX)){var _=wMV(_1eF,_1eD),_1f0=E(_1eM);return new F(function(){return _1dU(_1eG,_1eH,_1eI,_1eJ,_1eK+4|0,_1eL,_1f0[1],_1f0[2],_1f0[3],_1f0[4],_1f0[5],_1f0[6],_);});}else{var _=wMV(_1eF,_1ex),_1f1=E(_1eM);return new F(function(){return _1dm(_1eG,_1eH,_1eI,_1eJ,_1eK,_1eL,_1f1[1],_1f1[2],_1f1[3],_1f1[4],_1f1[5],_1f1[6],_);});}}else{var _=wMV(_1eF,_1ex),_1f2=E(_1eM);return new F(function(){return _1dm(_1eG,_1eH,_1eI,_1eJ,_1eK,_1eL,_1f2[1],_1f2[2],_1f2[3],_1f2[4],_1f2[5],_1f2[6],_);});}}else{var _=wMV(_1eF,_1ex),_1f3=E(_1eM);return new F(function(){return _1dm(_1eG,_1eH,_1eI,_1eJ,_1eK,_1eL,_1f3[1],_1f3[2],_1f3[3],_1f3[4],_1f3[5],_1f3[6],_);});}}else{var _=wMV(_1eF,_1ex),_1f4=E(_1eM);return new F(function(){return _1dm(_1eG,_1eH,_1eI,_1eJ,_1eK,_1eL,_1f4[1],_1f4[2],_1f4[3],_1f4[4],_1f4[5],_1f4[6],_);});}};if(!E(_1eR)){if(!E(_1eT)){if(E(_1eV)==254){if(E(_1eX)==255){var _=wMV(_1eF,_1ex),_1f5=E(_1eM);return new F(function(){return _1dm(_1eG,_1eH,_1eI,_1eJ,_1eK+4|0,_1eL,_1f5[1],_1f5[2],_1f5[3],_1f5[4],_1f5[5],_1f5[6],_);});}else{return new F(function(){return _1eY(_);});}}else{return new F(function(){return _1eY(_);});}}else{return new F(function(){return _1eY(_);});}}else{return new F(function(){return _1eY(_);});}}else{return [0,_16l,[0,_1eG,_1eH,_1eI,_1eJ,_1eK,_1eL],_1eM];}}else{return new F(function(){return A(_1eP[1],[[0,_1eG,_1eH,_1eI,_1eJ,_1eK,_1eL],_1eM,_]);});}},_1f6=function(_){return _c;},_1f7=new T(function(){return B(unCStr("UTF-32"));}),_1f8=function(_1f9){return [0,_1f7,function(_){var _1fa=nMV(_5h),_1fb=_1fa;return [0,function(_1fc,_1fd,_){var _1fe=E(_1fc);return new F(function(){return _1eE(_1fb,_1fe[1],_1fe[2],_1fe[3],_1fe[4],_1fe[5],_1fe[6],_1fd,_);});},function(_1ff,_1fg,_){return new F(function(){return _18R(_1f9,_1ff,_1fg,_);});},_1f6,function(_){return new F(function(){return rMV(_1fb);});},function(_1fh,_){var _=wMV(_1fb,_1fh);return _c;}];},function(_){var _1fi=nMV(_5E),_1fj=_1fi;return [0,function(_1fk,_1fl,_){var _1fm=E(_1fl);return new F(function(){return _1d9(_1fj,_1fk,_1fm[1],_1fm[2],_1fm[3],_1fm[4],_1fm[5],_1fm[6],_);});},function(_1ff,_1fg,_){return new F(function(){return _19i(_1f9,_1ff,_1fg,_);});},_1f6,function(_){return new F(function(){return rMV(_1fj);});},function(_1fn,_){var _=wMV(_1fj,_1fn);return _c;}];}];},_1fo=function(_1fp,_1fq,_){var _1fr=E(_1fp),_1fs=E(_1fq);return new F(function(){return _1cN(_1fr[1],_1fr[2],_1fr[3],_1fr[4],_1fr[5],_1fr[6],_1fs[1],_1fs[2],_1fs[3],_1fs[4],_1fs[5],_1fs[6],_);});},_1ft=function(_1fu,_){return _c;},_1fv=new T(function(){return B(unCStr("UTF-32BE"));}),_1fw=function(_1fx){return [0,_1fv,function(_){return [0,_1es,function(_1ff,_1fg,_){return new F(function(){return _18R(_1fx,_1ff,_1fg,_);});},_1f6,_1f6,_1ft];},function(_){return [0,_1fo,function(_1ff,_1fg,_){return new F(function(){return _19i(_1fx,_1ff,_1fg,_);});},_1f6,_1f6,_1ft];}];},_1fy=function(_1fz,_1fA,_1fB,_1fC,_1fD,_1fE,_1fF,_1fG,_1fH,_1fI,_1fJ,_1fK,_){var _1fL=[0,_1fz,_1fA,_1fB,_1fC,0,0],_1fM=function(_1fN,_1fO,_){if(_1fN<_1fE){if((_1fI-_1fO|0)>=4){var _1fP=readOffAddr("w32",4,_1fz,_1fN),_1fQ=_1fP,_=0,_1fR=_1fQ,_1fS=function(_1fT){if(56320>_1fR){var _=writeOffAddr("w8",1,plusAddr(_1fF,_1fO),0,_1fR>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_1fF,_1fO+1|0),0,_1fR>>8>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_1fF,_1fO+2|0),0,_1fR>>16>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_1fF,_1fO+3|0),0,_1fR>>24>>>0&255),_=0;return new F(function(){return _1fM(_1fN+1|0,_1fO+4|0,_);});}else{if(_1fR>57343){var _=writeOffAddr("w8",1,plusAddr(_1fF,_1fO),0,_1fR>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_1fF,_1fO+1|0),0,_1fR>>8>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_1fF,_1fO+2|0),0,_1fR>>16>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_1fF,_1fO+3|0),0,_1fR>>24>>>0&255),_=0;return new F(function(){return _1fM(_1fN+1|0,_1fO+4|0,_);});}else{return [0,_19v,new T(function(){return _1fN!=_1fE?[0,_1fz,_1fA,_1fB,_1fC,_1fN,_1fE]:E(_1fL);}),[0,_1fF,_1fG,_1fH,_1fI,_1fJ,_1fO]];}}};if(55296>_1fR){return new F(function(){return _1fS(_);});}else{return _1fR>56319?B(_1fS(_)):[0,_19v,new T(function(){return _1fN!=_1fE?[0,_1fz,_1fA,_1fB,_1fC,_1fN,_1fE]:E(_1fL);}),[0,_1fF,_1fG,_1fH,_1fI,_1fJ,_1fO]];}}else{return [0,_16m,new T(function(){return _1fN!=_1fE?[0,_1fz,_1fA,_1fB,_1fC,_1fN,_1fE]:E(_1fL);}),[0,_1fF,_1fG,_1fH,_1fI,_1fJ,_1fO]];}}else{return [0,_16l,new T(function(){return _1fN!=_1fE?[0,_1fz,_1fA,_1fB,_1fC,_1fN,_1fE]:E(_1fL);}),[0,_1fF,_1fG,_1fH,_1fI,_1fJ,_1fO]];}};return new F(function(){return _1fM(_1fD,_1fK,_);});},_1fU=function(_1fV,_1fW,_){var _1fX=E(_1fV),_1fY=E(_1fW);return new F(function(){return _1fy(_1fX[1],_1fX[2],_1fX[3],_1fX[4],_1fX[5],_1fX[6],_1fY[1],_1fY[2],_1fY[3],_1fY[4],_1fY[5],_1fY[6],_);});},_1fZ=new T(function(){return B(unCStr("UTF-32LE"));}),_1g0=function(_1g1){return [0,_1fZ,function(_){return [0,_1ey,function(_1ff,_1fg,_){return new F(function(){return _18R(_1g1,_1ff,_1fg,_);});},_1f6,_1f6,_1ft];},function(_){return [0,_1fU,function(_1ff,_1fg,_){return new F(function(){return _19i(_1g1,_1ff,_1fg,_);});},_1f6,_1f6,_1ft];}];},_1g2=function(_1g3,_1g4,_1g5,_1g6,_1g7,_1g8,_1g9,_1ga,_1gb,_1gc,_1gd,_1ge,_){var _1gf=[0,_1g3,_1g4,_1g5,_1g6,0,0],_1gg=function(_1gh,_1gi,_){while(1){var _1gj=(function(_1gk,_1gl,_){if(_1gl<_1gc){if(_1gk<_1g8){var _1gm=readOffAddr("w32",4,_1g3,_1gk),_1gn=_1gm,_=0,_1go=_1gn;if(_1go>127){if(_1go>2047){if(_1go>65535){if((_1gc-_1gl|0)>=4){var _=writeOffAddr("w8",1,plusAddr(_1g9,_1gl),0,((_1go>>18)+240|0)>>>0&255),_=0,_1gp=63>>>0,_=writeOffAddr("w8",1,plusAddr(_1g9,_1gl+1|0),0,(((_1go>>12>>>0&_1gp)>>>0&4294967295)+128|0)>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_1g9,_1gl+2|0),0,(((_1go>>6>>>0&_1gp)>>>0&4294967295)+128|0)>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_1g9,_1gl+3|0),0,(((_1go>>>0&_1gp)>>>0&4294967295)+128|0)>>>0&255),_=0,_1gq=_1gk+1|0,_1gr=_1gl+4|0;_1gh=_1gq;_1gi=_1gr;return null;}else{return [0,_16m,new T(function(){return _1gk!=_1g8?[0,_1g3,_1g4,_1g5,_1g6,_1gk,_1g8]:E(_1gf);}),[0,_1g9,_1ga,_1gb,_1gc,_1gd,_1gl]];}}else{var _1gs=function(_1gt){var _1gu=function(_1gv){if((_1gc-_1gl|0)>=3){var _=writeOffAddr("w8",1,plusAddr(_1g9,_1gl),0,((_1go>>12)+224|0)>>>0&255),_=0,_1gw=63>>>0,_=writeOffAddr("w8",1,plusAddr(_1g9,_1gl+1|0),0,(((_1go>>6>>>0&_1gw)>>>0&4294967295)+128|0)>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_1g9,_1gl+2|0),0,(((_1go>>>0&_1gw)>>>0&4294967295)+128|0)>>>0&255),_=0;return new F(function(){return _1gg(_1gk+1|0,_1gl+3|0,_);});}else{return [0,_16m,new T(function(){return _1gk!=_1g8?[0,_1g3,_1g4,_1g5,_1g6,_1gk,_1g8]:E(_1gf);}),[0,_1g9,_1ga,_1gb,_1gc,_1gd,_1gl]];}};if(56320>_1go){return new F(function(){return _1gu(_);});}else{return _1go>57343?B(_1gu(_)):[0,_19v,new T(function(){return _1gk!=_1g8?[0,_1g3,_1g4,_1g5,_1g6,_1gk,_1g8]:E(_1gf);}),[0,_1g9,_1ga,_1gb,_1gc,_1gd,_1gl]];}};if(55296>_1go){return new F(function(){return _1gs(_);});}else{return _1go>56319?B(_1gs(_)):[0,_19v,new T(function(){return _1gk!=_1g8?[0,_1g3,_1g4,_1g5,_1g6,_1gk,_1g8]:E(_1gf);}),[0,_1g9,_1ga,_1gb,_1gc,_1gd,_1gl]];}}}else{if((_1gc-_1gl|0)>=2){var _=writeOffAddr("w8",1,plusAddr(_1g9,_1gl),0,((_1go>>6)+192|0)>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_1g9,_1gl+1|0),0,(((_1go>>>0&63>>>0)>>>0&4294967295)+128|0)>>>0&255),_=0,_1gq=_1gk+1|0,_1gr=_1gl+2|0;_1gh=_1gq;_1gi=_1gr;return null;}else{return [0,_16m,new T(function(){return _1gk!=_1g8?[0,_1g3,_1g4,_1g5,_1g6,_1gk,_1g8]:E(_1gf);}),[0,_1g9,_1ga,_1gb,_1gc,_1gd,_1gl]];}}}else{var _=writeOffAddr("w8",1,plusAddr(_1g9,_1gl),0,_1go>>>0&255),_=0,_1gq=_1gk+1|0,_1gr=_1gl+1|0;_1gh=_1gq;_1gi=_1gr;return null;}}else{return [0,_16l,new T(function(){return _1gk!=_1g8?[0,_1g3,_1g4,_1g5,_1g6,_1gk,_1g8]:E(_1gf);}),[0,_1g9,_1ga,_1gb,_1gc,_1gd,_1gl]];}}else{return [0,_16m,new T(function(){return _1gk!=_1g8?[0,_1g3,_1g4,_1g5,_1g6,_1gk,_1g8]:E(_1gf);}),[0,_1g9,_1ga,_1gb,_1gc,_1gd,_1gl]];}})(_1gh,_1gi,_);if(_1gj!=null){return _1gj;}}};return new F(function(){return _1gg(_1g7,_1ge,_);});},_1gx=function(_1gy,_1gz,_){var _1gA=E(_1gy),_1gB=E(_1gz);return new F(function(){return _1g2(_1gA[1],_1gA[2],_1gA[3],_1gA[4],_1gA[5],_1gA[6],_1gB[1],_1gB[2],_1gB[3],_1gB[4],_1gB[5],_1gB[6],_);});},_1gC=function(_1gD,_){return _c;},_1gE=function(_){return _c;},_1gF=function(_1gG,_1gH,_1gI,_1gJ,_1gK,_1gL,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gR,_){var _1gS=[0,_1gG,_1gH,_1gI,_1gJ,0,0],_1gT=function(_1gU,_1gV,_){while(1){var _1gW=(function(_1gX,_1gY,_){if(_1gY<_1gP){if(_1gX<_1gL){var _1gZ=readOffAddr("w8",1,plusAddr(_1gG,_1gX),0),_1h0=_1gZ,_=0;if(_1h0>127){var _1h1=function(_1h2){var _1h3=function(_1h4){if(_1h0<240){return [0,_19v,new T(function(){return _1gX!=_1gL?[0,_1gG,_1gH,_1gI,_1gJ,_1gX,_1gL]:E(_1gS);}),[0,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gY]];}else{switch(_1gL-_1gX|0){case 1:return [0,_16l,new T(function(){return _1gX!=_1gL?[0,_1gG,_1gH,_1gI,_1gJ,_1gX,_1gL]:E(_1gS);}),[0,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gY]];case 2:var _1h5=readOffAddr("w8",1,plusAddr(_1gG,_1gX+1|0),0),_1h6=_1h5,_=0,_1h7=function(_1h8){var _1h9=function(_1ha){return E(_1h0)==244?_1h6<128?[0,_19v,new T(function(){return _1gX!=_1gL?[0,_1gG,_1gH,_1gI,_1gJ,_1gX,_1gL]:E(_1gS);}),[0,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gY]]:_1h6>143?[0,_19v,new T(function(){return _1gX!=_1gL?[0,_1gG,_1gH,_1gI,_1gJ,_1gX,_1gL]:E(_1gS);}),[0,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gY]]:[0,_16l,new T(function(){return _1gX!=_1gL?[0,_1gG,_1gH,_1gI,_1gJ,_1gX,_1gL]:E(_1gS);}),[0,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gY]]:[0,_19v,new T(function(){return _1gX!=_1gL?[0,_1gG,_1gH,_1gI,_1gJ,_1gX,_1gL]:E(_1gS);}),[0,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gY]];};if(_1h0<241){return new F(function(){return _1h9(_);});}else{if(_1h0>243){return new F(function(){return _1h9(_);});}else{if(_1h6<128){return new F(function(){return _1h9(_);});}else{return _1h6>191?B(_1h9(_)):[0,_16l,new T(function(){return _1gX!=_1gL?[0,_1gG,_1gH,_1gI,_1gJ,_1gX,_1gL]:E(_1gS);}),[0,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gY]];}}}};if(E(_1h0)==240){if(_1h6<144){return new F(function(){return _1h7(_);});}else{return _1h6>191?B(_1h7(_)):[0,_16l,new T(function(){return _1gX!=_1gL?[0,_1gG,_1gH,_1gI,_1gJ,_1gX,_1gL]:E(_1gS);}),[0,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gY]];}}else{return new F(function(){return _1h7(_);});}break;case 3:var _1hb=readOffAddr("w8",1,plusAddr(_1gG,_1gX+1|0),0),_1hc=_1hb,_=0,_1hd=readOffAddr("w8",1,plusAddr(_1gG,_1gX+2|0),0),_1he=_1hd,_=0,_1hf=function(_1hg){var _1hh=function(_1hi){return E(_1h0)==244?_1hc<128?[0,_19v,new T(function(){return _1gX!=_1gL?[0,_1gG,_1gH,_1gI,_1gJ,_1gX,_1gL]:E(_1gS);}),[0,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gY]]:_1hc>143?[0,_19v,new T(function(){return _1gX!=_1gL?[0,_1gG,_1gH,_1gI,_1gJ,_1gX,_1gL]:E(_1gS);}),[0,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gY]]:_1he<128?[0,_19v,new T(function(){return _1gX!=_1gL?[0,_1gG,_1gH,_1gI,_1gJ,_1gX,_1gL]:E(_1gS);}),[0,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gY]]:_1he>191?[0,_19v,new T(function(){return _1gX!=_1gL?[0,_1gG,_1gH,_1gI,_1gJ,_1gX,_1gL]:E(_1gS);}),[0,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gY]]:[0,_16l,new T(function(){return _1gX!=_1gL?[0,_1gG,_1gH,_1gI,_1gJ,_1gX,_1gL]:E(_1gS);}),[0,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gY]]:[0,_19v,new T(function(){return _1gX!=_1gL?[0,_1gG,_1gH,_1gI,_1gJ,_1gX,_1gL]:E(_1gS);}),[0,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gY]];};if(_1h0<241){return new F(function(){return _1hh(_);});}else{if(_1h0>243){return new F(function(){return _1hh(_);});}else{if(_1hc<128){return new F(function(){return _1hh(_);});}else{if(_1hc>191){return new F(function(){return _1hh(_);});}else{if(_1he<128){return new F(function(){return _1hh(_);});}else{return _1he>191?B(_1hh(_)):[0,_16l,new T(function(){return _1gX!=_1gL?[0,_1gG,_1gH,_1gI,_1gJ,_1gX,_1gL]:E(_1gS);}),[0,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gY]];}}}}}};if(E(_1h0)==240){if(_1hc<144){return new F(function(){return _1hf(_);});}else{if(_1hc>191){return new F(function(){return _1hf(_);});}else{if(_1he<128){return new F(function(){return _1hf(_);});}else{return _1he>191?B(_1hf(_)):[0,_16l,new T(function(){return _1gX!=_1gL?[0,_1gG,_1gH,_1gI,_1gJ,_1gX,_1gL]:E(_1gS);}),[0,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gY]];}}}}else{return new F(function(){return _1hf(_);});}break;default:var _1hj=readOffAddr("w8",1,plusAddr(_1gG,_1gX+1|0),0),_1hk=_1hj,_=0,_1hl=readOffAddr("w8",1,plusAddr(_1gG,_1gX+2|0),0),_1hm=_1hl,_=0,_1hn=readOffAddr("w8",1,plusAddr(_1gG,_1gX+3|0),0),_1ho=_1hn,_=0,_1hp=function(_1hq){var _=writeOffAddr("w32",4,_1gM,_1gY,(((((_1h0&4294967295)-240|0)<<18)+(((_1hk&4294967295)-128|0)<<12)|0)+(((_1hm&4294967295)-128|0)<<6)|0)+((_1ho&4294967295)-128|0)|0),_=0;return new F(function(){return _1gT(_1gX+4|0,_1gY+1|0,_);});},_1hr=function(_1hs){var _1ht=function(_1hu){return E(_1h0)==244?_1hk<128?[0,_19v,new T(function(){return _1gX!=_1gL?[0,_1gG,_1gH,_1gI,_1gJ,_1gX,_1gL]:E(_1gS);}),[0,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gY]]:_1hk>143?[0,_19v,new T(function(){return _1gX!=_1gL?[0,_1gG,_1gH,_1gI,_1gJ,_1gX,_1gL]:E(_1gS);}),[0,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gY]]:_1hm<128?[0,_19v,new T(function(){return _1gX!=_1gL?[0,_1gG,_1gH,_1gI,_1gJ,_1gX,_1gL]:E(_1gS);}),[0,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gY]]:_1hm>191?[0,_19v,new T(function(){return _1gX!=_1gL?[0,_1gG,_1gH,_1gI,_1gJ,_1gX,_1gL]:E(_1gS);}),[0,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gY]]:_1ho<128?[0,_19v,new T(function(){return _1gX!=_1gL?[0,_1gG,_1gH,_1gI,_1gJ,_1gX,_1gL]:E(_1gS);}),[0,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gY]]:_1ho>191?[0,_19v,new T(function(){return _1gX!=_1gL?[0,_1gG,_1gH,_1gI,_1gJ,_1gX,_1gL]:E(_1gS);}),[0,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gY]]:B(_1hp(_)):[0,_19v,new T(function(){return _1gX!=_1gL?[0,_1gG,_1gH,_1gI,_1gJ,_1gX,_1gL]:E(_1gS);}),[0,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gY]];};if(_1h0<241){return new F(function(){return _1ht(_);});}else{if(_1h0>243){return new F(function(){return _1ht(_);});}else{if(_1hk<128){return new F(function(){return _1ht(_);});}else{if(_1hk>191){return new F(function(){return _1ht(_);});}else{if(_1hm<128){return new F(function(){return _1ht(_);});}else{if(_1hm>191){return new F(function(){return _1ht(_);});}else{if(_1ho<128){return new F(function(){return _1ht(_);});}else{return _1ho>191?B(_1ht(_)):B(_1hp(_));}}}}}}}};if(E(_1h0)==240){if(_1hk<144){return new F(function(){return _1hr(_);});}else{if(_1hk>191){return new F(function(){return _1hr(_);});}else{if(_1hm<128){return new F(function(){return _1hr(_);});}else{if(_1hm>191){return new F(function(){return _1hr(_);});}else{if(_1ho<128){return new F(function(){return _1hr(_);});}else{return _1ho>191?B(_1hr(_)):B(_1hp(_));}}}}}}else{return new F(function(){return _1hr(_);});}}}};if(_1h0<224){return new F(function(){return _1h3(_);});}else{if(_1h0>239){return new F(function(){return _1h3(_);});}else{switch(_1gL-_1gX|0){case 1:return [0,_16l,new T(function(){return _1gX!=_1gL?[0,_1gG,_1gH,_1gI,_1gJ,_1gX,_1gL]:E(_1gS);}),[0,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gY]];case 2:var _1hv=readOffAddr("w8",1,plusAddr(_1gG,_1gX+1|0),0),_1hw=_1hv,_=0,_1hx=function(_1hy){var _1hz=function(_1hA){var _1hB=function(_1hC){return _1h0<238?[0,_19v,new T(function(){return _1gX!=_1gL?[0,_1gG,_1gH,_1gI,_1gJ,_1gX,_1gL]:E(_1gS);}),[0,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gY]]:_1hw<128?[0,_19v,new T(function(){return _1gX!=_1gL?[0,_1gG,_1gH,_1gI,_1gJ,_1gX,_1gL]:E(_1gS);}),[0,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gY]]:_1hw>191?[0,_19v,new T(function(){return _1gX!=_1gL?[0,_1gG,_1gH,_1gI,_1gJ,_1gX,_1gL]:E(_1gS);}),[0,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gY]]:[0,_16l,new T(function(){return _1gX!=_1gL?[0,_1gG,_1gH,_1gI,_1gJ,_1gX,_1gL]:E(_1gS);}),[0,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gY]];};if(E(_1h0)==237){if(_1hw<128){return new F(function(){return _1hB(_);});}else{return _1hw>159?B(_1hB(_)):[0,_16l,new T(function(){return _1gX!=_1gL?[0,_1gG,_1gH,_1gI,_1gJ,_1gX,_1gL]:E(_1gS);}),[0,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gY]];}}else{return new F(function(){return _1hB(_);});}};if(_1h0<225){return new F(function(){return _1hz(_);});}else{if(_1h0>236){return new F(function(){return _1hz(_);});}else{if(_1hw<128){return new F(function(){return _1hz(_);});}else{return _1hw>191?B(_1hz(_)):[0,_16l,new T(function(){return _1gX!=_1gL?[0,_1gG,_1gH,_1gI,_1gJ,_1gX,_1gL]:E(_1gS);}),[0,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gY]];}}}};if(E(_1h0)==224){if(_1hw<160){return new F(function(){return _1hx(_);});}else{return _1hw>191?B(_1hx(_)):[0,_16l,new T(function(){return _1gX!=_1gL?[0,_1gG,_1gH,_1gI,_1gJ,_1gX,_1gL]:E(_1gS);}),[0,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gY]];}}else{return new F(function(){return _1hx(_);});}break;default:var _1hD=readOffAddr("w8",1,plusAddr(_1gG,_1gX+1|0),0),_1hE=_1hD,_=0,_1hF=readOffAddr("w8",1,plusAddr(_1gG,_1gX+2|0),0),_1hG=_1hF,_=0,_1hH=function(_1hI){var _=writeOffAddr("w32",4,_1gM,_1gY,((((_1h0&4294967295)-224|0)<<12)+(((_1hE&4294967295)-128|0)<<6)|0)+((_1hG&4294967295)-128|0)|0),_=0;return new F(function(){return _1gT(_1gX+3|0,_1gY+1|0,_);});},_1hJ=function(_1hK){var _1hL=function(_1hM){var _1hN=function(_1hO){return _1h0<238?[0,_19v,new T(function(){return _1gX!=_1gL?[0,_1gG,_1gH,_1gI,_1gJ,_1gX,_1gL]:E(_1gS);}),[0,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gY]]:_1hE<128?[0,_19v,new T(function(){return _1gX!=_1gL?[0,_1gG,_1gH,_1gI,_1gJ,_1gX,_1gL]:E(_1gS);}),[0,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gY]]:_1hE>191?[0,_19v,new T(function(){return _1gX!=_1gL?[0,_1gG,_1gH,_1gI,_1gJ,_1gX,_1gL]:E(_1gS);}),[0,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gY]]:_1hG<128?[0,_19v,new T(function(){return _1gX!=_1gL?[0,_1gG,_1gH,_1gI,_1gJ,_1gX,_1gL]:E(_1gS);}),[0,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gY]]:_1hG>191?[0,_19v,new T(function(){return _1gX!=_1gL?[0,_1gG,_1gH,_1gI,_1gJ,_1gX,_1gL]:E(_1gS);}),[0,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gY]]:B(_1hH(_));};if(E(_1h0)==237){if(_1hE<128){return new F(function(){return _1hN(_);});}else{if(_1hE>159){return new F(function(){return _1hN(_);});}else{if(_1hG<128){return new F(function(){return _1hN(_);});}else{return _1hG>191?B(_1hN(_)):B(_1hH(_));}}}}else{return new F(function(){return _1hN(_);});}};if(_1h0<225){return new F(function(){return _1hL(_);});}else{if(_1h0>236){return new F(function(){return _1hL(_);});}else{if(_1hE<128){return new F(function(){return _1hL(_);});}else{if(_1hE>191){return new F(function(){return _1hL(_);});}else{if(_1hG<128){return new F(function(){return _1hL(_);});}else{return _1hG>191?B(_1hL(_)):B(_1hH(_));}}}}}};if(E(_1h0)==224){if(_1hE<160){return new F(function(){return _1hJ(_);});}else{if(_1hE>191){return new F(function(){return _1hJ(_);});}else{if(_1hG<128){return new F(function(){return _1hJ(_);});}else{return _1hG>191?B(_1hJ(_)):B(_1hH(_));}}}}else{return new F(function(){return _1hJ(_);});}}}}};if(_1h0<192){return new F(function(){return _1h1(_);});}else{if(_1h0>223){return new F(function(){return _1h1(_);});}else{if((_1gL-_1gX|0)>=2){var _1hP=readOffAddr("w8",1,plusAddr(_1gG,_1gX+1|0),0),_1hQ=_1hP,_=0;if(_1hQ>=128){if(_1hQ<192){var _=writeOffAddr("w32",4,_1gM,_1gY,(((_1h0&4294967295)-192|0)<<6)+((_1hQ&4294967295)-128|0)|0),_=0,_1hR=_1gX+2|0,_1hS=_1gY+1|0;_1gU=_1hR;_1gV=_1hS;return null;}else{return [0,_19v,new T(function(){return _1gX!=_1gL?[0,_1gG,_1gH,_1gI,_1gJ,_1gX,_1gL]:E(_1gS);}),[0,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gY]];}}else{return [0,_19v,new T(function(){return _1gX!=_1gL?[0,_1gG,_1gH,_1gI,_1gJ,_1gX,_1gL]:E(_1gS);}),[0,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gY]];}}else{return [0,_16l,new T(function(){return _1gX!=_1gL?[0,_1gG,_1gH,_1gI,_1gJ,_1gX,_1gL]:E(_1gS);}),[0,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gY]];}}}}else{var _=writeOffAddr("w32",4,_1gM,_1gY,_1h0&4294967295),_=0,_1hR=_1gX+1|0,_1hS=_1gY+1|0;_1gU=_1hR;_1gV=_1hS;return null;}}else{return [0,_16l,new T(function(){return _1gX!=_1gL?[0,_1gG,_1gH,_1gI,_1gJ,_1gX,_1gL]:E(_1gS);}),[0,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gY]];}}else{return [0,_16m,new T(function(){return _1gX!=_1gL?[0,_1gG,_1gH,_1gI,_1gJ,_1gX,_1gL]:E(_1gS);}),[0,_1gM,_1gN,_1gO,_1gP,_1gQ,_1gY]];}})(_1gU,_1gV,_);if(_1gW!=null){return _1gW;}}};return new F(function(){return _1gT(_1gK,_1gR,_);});},_1hT=function(_1hU,_1hV,_){var _1hW=E(_1hU),_1hX=E(_1hV);return new F(function(){return _1gF(_1hW[1],_1hW[2],_1hW[3],_1hW[4],_1hW[5],_1hW[6],_1hX[1],_1hX[2],_1hX[3],_1hX[4],_1hX[5],_1hX[6],_);});},_1hY=new T(function(){return B(unCStr("UTF-8"));}),_1hZ=function(_1i0){return [0,_1hY,function(_){return [0,_1hT,function(_1i1,_1i2,_){return new F(function(){return _18R(_1i0,_1i1,_1i2,_);});},_1gE,_1gE,_1gC];},function(_){return [0,_1gx,function(_1i1,_1i2,_){return new F(function(){return _19i(_1i0,_1i1,_1i2,_);});},_1gE,_1gE,_1gC];}];},_1i3=function(_1i4,_1i5,_){var _1i6=B(_16d(_1i5));return !B(_hS(_1i6,_164))?!B(_hS(_1i6,_163))?!B(_hS(_1i6,_162))?!B(_hS(_1i6,_168))?!B(_hS(_1i6,_167))?!B(_hS(_1i6,_166))?!B(_hS(_1i6,_165))?B(_19o(_1i4,_1i5,_)):new T(function(){return B(_1hZ(_1i4));}):new T(function(){return B(_1g0(_1i4));}):new T(function(){return B(_1fw(_1i4));}):new T(function(){return B(_1f8(_1i4));}):new T(function(){return B(_1cL(_1i4));}):new T(function(){return B(_1ca(_1i4));}):new T(function(){return B(_1bM(_1i4));});},_1i7=function(_1i8,_){var _1i9=B((function(_1ia,_){while(1){var _1ib=readOffAddr("i8",1,_1i8,_1ia),_1ic=_1ib;if(!E(_1ic)){return [0,_1ia];}else{var _1id=_1ia+1|0;_1ia=_1id;continue;}}})(0,_)),_1ie=_1i9,_1if=E(_1ie)[1];if(_1if>0){return new F(function(){return (function(_1ig,_1ih,_){while(1){var _1ii=readOffAddr("i8",1,_1i8,_1ih),_1ij=_1ii;if(_1ih>0){var _1ik=[1,[0,_1ij>>>0&255&4294967295],_1ig],_1il=_1ih-1|0;_1ig=_1ik;_1ih=_1il;continue;}else{return [1,[0,_1ij>>>0&255&4294967295],_1ig];}}})(_1g,_1if-1|0,_);});}else{return _1g;}},_1im=function(_){var _=0,_1in=localeEncoding(),_1io=_1in;return new F(function(){return _1i7(_1io,_);});},_1ip=new T(function(){return B(_6(_1im));}),_1iq=function(_){var _=0;return new F(function(){return _1i3(_161,_1ip,_);});},_1ir=new T(function(){return B(_6(_1iq));}),_1is=function(_){var _=0,_1it=nMV(_1ir),_1iu=_1it;return [0,function(_){return new F(function(){return rMV(_1iu);});},function(_1iv,_){var _=wMV(_1iu,_1iv);return _c;}];},_1iw=new T(function(){return B(_6(_1is));}),_17A=function(_1ix,_1iy,_1iz,_1iA){return new F(function(){return _6(function(_){var _=0,_1iB=strerror(_1iy),_1iC=_1iB,_1iD=B(A(E(_1iw)[1],[_])),_1iE=_1iD,_1iF=B(_15g(_1iE,_1iC,_)),_1iG=_1iF;return [0,_1iz,new T(function(){switch(E(_1iy)){case 1:var _1iH=6;break;case 2:var _1iH=1;break;case 3:var _1iH=1;break;case 4:var _1iH=18;break;case 5:var _1iH=14;break;case 6:var _1iH=1;break;case 7:var _1iH=3;break;case 8:var _1iH=12;break;case 9:var _1iH=12;break;case 10:var _1iH=1;break;case 11:var _1iH=3;break;case 12:var _1iH=3;break;case 13:var _1iH=6;break;case 15:var _1iH=12;break;case 16:var _1iH=2;break;case 17:var _1iH=0;break;case 18:var _1iH=15;break;case 19:var _1iH=15;break;case 20:var _1iH=13;break;case 21:var _1iH=13;break;case 22:var _1iH=12;break;case 23:var _1iH=3;break;case 24:var _1iH=3;break;case 25:var _1iH=5;break;case 26:var _1iH=2;break;case 27:var _1iH=6;break;case 28:var _1iH=3;break;case 29:var _1iH=15;break;case 30:var _1iH=6;break;case 31:var _1iH=3;break;case 32:var _1iH=17;break;case 33:var _1iH=12;break;case 34:var _1iH=15;break;case 35:var _1iH=2;break;case 36:var _1iH=12;break;case 37:var _1iH=3;break;case 38:var _1iH=15;break;case 39:var _1iH=8;break;case 40:var _1iH=12;break;case 42:var _1iH=1;break;case 43:var _1iH=17;break;case 60:var _1iH=12;break;case 61:var _1iH=1;break;case 62:var _1iH=16;break;case 63:var _1iH=3;break;case 64:var _1iH=1;break;case 66:var _1iH=5;break;case 67:var _1iH=17;break;case 69:var _1iH=8;break;case 70:var _1iH=17;break;case 71:var _1iH=10;break;case 72:var _1iH=15;break;case 74:var _1iH=13;break;case 78:var _1iH=17;break;case 84:var _1iH=12;break;case 87:var _1iH=3;break;case 88:var _1iH=12;break;case 89:var _1iH=12;break;case 90:var _1iH=3;break;case 91:var _1iH=10;break;case 92:var _1iH=15;break;case 93:var _1iH=10;break;case 94:var _1iH=15;break;case 95:var _1iH=15;break;case 96:var _1iH=15;break;case 97:var _1iH=15;break;case 98:var _1iH=2;break;case 99:var _1iH=15;break;case 100:var _1iH=17;break;case 101:var _1iH=1;break;case 102:var _1iH=17;break;case 104:var _1iH=17;break;case 105:var _1iH=3;break;case 106:var _1iH=0;break;case 107:var _1iH=12;break;case 108:var _1iH=5;break;case 109:var _1iH=3;break;case 110:var _1iH=16;break;case 111:var _1iH=1;break;case 112:var _1iH=1;break;case 113:var _1iH=1;break;case 114:var _1iH=0;break;case 115:var _1iH=0;break;case 116:var _1iH=17;break;case 122:var _1iH=6;break;default:var _1iH=11;}return _1iH;}),_1ix,_1iG,[1,[0,_1iy]],_1iA];});});},_1iI=new T(function(){return B(unCStr("gettimeofday"));}),_1iJ=function(_){var _1iK=newByteArr(8),_1iL=_1iK,_1iM=_1iL,_1iN=_1iM,_1iO=_1iN,_=writeOffAddr("i32",4,_1iO,0,0),_=writeOffAddr("i32",4,_1iO,1,0),_1iP=gettimeofday(_1iO,0),_1iQ=_1iP;if(E(_1iQ)==(-1)){var _1iR=__hscore_get_errno(),_1iS=_1iR;return new F(function(){return _16r(B(_17A(_1iI,_1iS,_5h,_5h)),_);});}else{var _1iT=readOffAddr("i32",4,_1iO,0),_1iU=_1iT,_1iV=readOffAddr("i32",4,_1iO,1),_1iW=_1iV,_=0;return [0,[0,_1iU],[0,_1iW]];}},_1iX=[1,I_fromBits([2808348672,232830643])],_1iY=function(_){var _1iZ=B(_1iJ(_)),_1j0=_1iZ;return new T(function(){var _1j1=E(_1j0);if(!B(_3j(_1iX,_13l))){var _1j2=B(_qx(B(_qP(B(_qN(E(_1j1[1])[1])),_110)),B(_125(B(_qP(B(_qP(B(_qN(E(_1j1[2])[1])),_110)),_110)),_1iX))));}else{var _1j2=E(_3i);}var _1j3=_1j2,_1j4=_1j3;return _1j4;});},_1j5=[0,12345],_1j6=function(_){var _=0,_1j7=B(_1iY(_)),_1j8=_1j7,_1j9=B(_111(E(B(_13M(_1j8))[2]),_Je,_110,_Je)),_1ja=_1j9[2];if(!B(_3j(_1ja,_SJ))){var _1jb=B(_12z(_1j9[1],_1ja)),_1jc=nMV(new T(function(){var _1jd=B(_11l(B(_s2(B(_qx(B(_qx(B(_qx(B(_qP(_1jb[1],_1j5)),_1jb[2])),_13S)),_SJ))))));return [0,_1jd[1],_1jd[2]];})),_1je=_1jc;return [0,_1je];}else{return E(_3i);}},_1jf=new T(function(){return B(_6(_1j6));}),_1jg=function(_){var _1jh=mMV(E(_1jf)[1],_10U),_1ji=_1jh,_1jj=E(_1ji);return E(_1jh);},_1jk=[0,2],_1jl=new T(function(){return B(_IM(_SE,_1jk));}),_1jm=new T(function(){return B(_IM(_1jk,_SE));}),_1jn=function(_1jo,_1jp,_1jq){while(1){var _1jr=(function(_1js,_1jt,_1ju){if(_1js<=_1jt){var _1jv=new T(function(){var _1jw=B(_10R(_1ju));return [0,_1jw[1],_1jw[2]];});return [0,new T(function(){var _1jx=E(_1jl)[1];return [0,E(_1jm)[1]*(_1jx*_1js+E(E(_1jv)[1])[1]*(_1jx*_1jt-_1jx*_1js))];}),new T(function(){return E(E(_1jv)[2]);})];}else{var _1jy=_1jt,_1jz=_1js,_1jA=_1ju;_1jo=_1jy;_1jp=_1jz;_1jq=_1jA;return null;}})(_1jo,_1jp,_1jq);if(_1jr!=null){return _1jr;}}},_1jB=function(_1jC,_1jD){var _1jE=E(_1jC),_1jF=E(_1jE[1])[1],_1jG=E(_1jE[2])[1];if(_1jF<=_1jG){var _1jH=new T(function(){var _1jI=B(_10R(_1jD));return [0,_1jI[1],_1jI[2]];});return [0,new T(function(){var _1jJ=E(_1jl)[1];return [0,E(_1jm)[1]*(_1jJ*_1jF+E(E(_1jH)[1])[1]*(_1jJ*_1jG-_1jJ*_1jF))];}),new T(function(){return E(E(_1jH)[2]);})];}else{return new F(function(){return _1jn(_1jG,_1jF,_1jD);});}},_1jK=function(_1jL,_){var _1jM=mMV(E(_1jf)[1],function(_1jN){var _1jO=new T(function(){var _1jP=B(_1jB(_1jL,_1jN));return [0,_1jP[2],_1jP[1]];}),_1jQ=new T(function(){return E(E(_1jO)[1]);});return [0,_1jQ,new T(function(){var _1jR=E(_1jQ);return E(E(_1jO)[2]);})];}),_1jS=_1jM,_1jT=E(_1jS);return E(_1jM);},_1jU=function(_1jV,_1jW){var _1jX=B(_SV(_1jV,_Yt,B(_Yy(E(_Yv)[1])),_Yu,_1jW));return [0,new T(function(){return [0,B(_WG(B(_Yy(B(_ZV(E(_ZS)[1],E(_1jX[1])[1]))))))/E(_ZR)[1]];}),_1jX[2]];},_1jY=function(_1jZ,_1k0){var _1k1=B(_1jU(_1jZ,_1k0));return [0,_1k1[1],_1k1[2]];},_1k2=function(_1k3,_1k4,_1k5){var _1k6=E(_1k4),_1k7=B(_1k8(_1k3,_1k6[1],_1k6[2],_1k5));return [0,_1k7[1],_1k7[2]];},_1k9=function(_1ka,_1kb,_1kc){var _1kd=E(_1kb),_1ke=_1kd[1],_1kf=_1kd[2],_1kg=B(_1k8(_1ka,_1ke,_1kf,_1kc));return [1,E(_1kg[1]),new T(function(){var _1kh=function(_1ki){var _1kj=B(_1k8(_1ka,_1ke,_1kf,_1ki));return [1,E(_1kj[1]),new T(function(){return B(_1kh(_1kj[2]));})];};return B(_1kh(_1kg[2]));})];},_1kk=function(_1kl,_1km){var _1kn=function(_1ko){var _1kp=B(_1jU(_1kl,_1ko));return [1,E(_1kp[1]),new T(function(){return B(_1kn(_1kp[2]));})];};return new F(function(){return _1kn(_1km);});},_1kq=new T(function(){return [0,_1k2,_1jY,_1k9,_1kk,_1jK,_1jg];}),_1kr=function(_1ks){return E(E(_1ks)[2]);},_1kt=function(_1ku){return E(E(_1ku)[1]);},_1kv=function(_1kw){return E(E(_1kw)[3]);},_1kx=function(_1ky){return E(E(_1ky)[5]);},_1kz=function(_1kA){return E(E(_1kA)[4]);},_1kB=[0,E(_SE),E(_1jk)],_1kC=[0,E(_1jk),E(_SE)],_1kD=function(_1kE){return E(E(_1kE)[2]);},_1kF=function(_1kG,_1kH,_1kI,_1kJ,_1kK,_1kL,_1kM){while(1){var _1kN=(function(_1kO,_1kP,_1kQ,_1kR,_1kS,_1kT,_1kU){var _1kV=E(_1kT),_1kW=_1kV[1],_1kX=_1kV[2];if(!B(A(_1kx,[_1kQ,_1kW,_1kX]))){var _1kY=new T(function(){return B(A(_1kD,[_1kR,_1kS,_1kU]));});return [0,new T(function(){return B(A(_1kr,[_1kP,new T(function(){return B(A(_1kz,[_1kO,_1kC]));}),new T(function(){return B(A(_1kt,[_1kP,new T(function(){return B(A(_1kr,[_1kP,new T(function(){return B(A(_1kz,[_1kO,_1kB]));}),_1kW]));}),new T(function(){return B(A(_1kr,[_1kP,new T(function(){return E(E(_1kY)[1]);}),new T(function(){return B(A(_1kv,[_1kP,new T(function(){return B(A(_1kr,[_1kP,new T(function(){return B(A(_1kz,[_1kO,_1kB]));}),_1kX]));}),new T(function(){return B(A(_1kr,[_1kP,new T(function(){return B(A(_1kz,[_1kO,_1kB]));}),_1kW]));})]));})]));})]));})]));}),new T(function(){return E(E(_1kY)[2]);})];}else{var _1kZ=_1kO,_1l0=_1kP,_1l1=_1kQ,_1l2=_1kR,_1l3=_1kS;_1kL=[0,_1kX,_1kW];var _1l4=_1kU;_1kG=_1kZ;_1kH=_1l0;_1kI=_1l1;_1kJ=_1l2;_1kK=_1l3;_1kM=_1l4;return null;}})(_1kG,_1kH,_1kI,_1kJ,_1kK,_1kL,_1kM);if(_1kN!=null){return _1kN;}}},_1k8=function(_1l5,_1l6,_1l7,_1l8){var _1l9=B(_1kF(_X6,_X2,_XF,_1kq,_1l5,[0,_1l6,_1l7],_1l8));return [0,_1l9[1],_1l9[2]];},_1la=function(_1lb,_1lc,_1ld){return [0,_1lc,new T(function(){var _1le=E(_1ld);if(!_1le[0]){var _1lf=[0];}else{var _1lg=B(_1la(_1lb,new T(function(){return B(A(_1lb,[_1lc,_1le[1]]));}),_1le[2])),_1lf=[1,_1lg[1],_1lg[2]];}return _1lf;})];},_1lh=new T(function(){return B(unCStr(": empty list"));}),_1li=new T(function(){return B(unCStr("Prelude."));}),_1lj=function(_1lk){return new F(function(){return err(B(_2J(_1li,new T(function(){return B(_2J(_1lk,_1lh));},1))));});},_1ll=new T(function(){return B(unCStr("head"));}),_1lm=new T(function(){return B(_1lj(_1ll));}),_1ln=[0,0],_1lo=function(_1lp,_1lq){var _1lr=E(_1lq);return [0,_1lr[1],new T(function(){return B(_VU(_1lr[2],E(_1lp)[2]));})];},_1ls=new T(function(){return B(unCStr("MonadRandom.fromList called with empty list"));}),_1lt=new T(function(){return B(err(_1ls));}),_1lu=function(_1lv,_){return [1,[0,_1g,_1lv]];},_1lw=[0,1],_1lx=function(_1ly){var _1lz=I_decodeDouble(_1ly);return [0,[1,_1lz[2]],_1lz[1]];},_1lA=new T(function(){var _1lB=newByteArr(256),_1lC=_1lB,_=_1lC["v"]["i8"][0]=8,_=B((function(_1lD,_1lE,_1lF,_){while(1){if(_1lF>=256){if(_1lD>=256){return E(_);}else{var _1lG=imul(2,_1lD)|0,_1lH=_1lE+1|0,_1lI=_1lD;_1lD=_1lG;_1lE=_1lH;_1lF=_1lI;continue;}}else{var _=_1lC["v"]["i8"][_1lF]=_1lE,_1lI=_1lF+_1lD|0;_1lF=_1lI;continue;}}})(2,0,1,_)),_1lJ=_1lC,_1lK=_1lJ;return [0,_1lK];}),_1lL=function(_1lM,_1lN){while(1){var _1lO=(function(_1lP,_1lQ){var _1lR=hs_int64ToInt(_1lP),_1lS=_1lR,_1lT=E(_1lA)[1]["v"]["i8"][(255&_1lS>>>0)>>>0&4294967295];if(_1lQ>_1lT){if(_1lT>=8){var _1lU=hs_uncheckedIShiftRA64(_1lP,8),_1lV=_1lU;_1lM=_1lV;var _1lW=_1lQ-8|0;_1lN=_1lW;return null;}else{return [0,new T(function(){var _1lX=hs_uncheckedIShiftRA64(_1lP,_1lT),_1lY=_1lX;return B(_Yy(_1lY));}),_1lQ-_1lT|0];}}else{return [0,new T(function(){var _1lZ=hs_uncheckedIShiftRA64(_1lP,_1lQ),_1m0=_1lZ;return B(_Yy(_1m0));}),0];}})(_1lM,_1lN);if(_1lO!=null){return _1lO;}}},_1m1=function(_1m2){return I_toInt(_1m2)>>>0;},_1m3=function(_1m4){var _1m5=E(_1m4);return _1m5[0]==0?_1m5[1]>>>0:B(_1m1(_1m5[1]));},_1m6=function(_1m7,_1m8){while(1){var _1m9=E(_1m7);if(!_1m9[0]){_1m7=[1,I_fromInt(_1m9[1])];continue;}else{return [1,I_shiftLeft(_1m9[1],_1m8)];}}},_1ma=function(_1mb){var _1mc=B(_1lx(_1mb)),_1md=_1mc[1],_1me=_1mc[2];if(_1me<0){var _1mf=function(_1mg){if(!_1mg){return [0,E(_1md),B(_1m6(_1lw, -_1me))];}else{var _1mh=B(_1lL(B(_Y7(_1md)), -_1me));return [0,E(_1mh[1]),B(_1m6(_1lw,_1mh[2]))];}};return (B(_1m3(_1md))&1)>>>0==0?B(_1mf(1)):B(_1mf(0));}else{return [0,B(_1m6(_1md,_1me)),_1lw];}},_1mi=function(_1mj,_){return [1,[0,new T(function(){var _1mk=B(_1ma(E(E(_1mj)[1])[1]));return [0,E(_1mk[1]),E(_1mk[2])];}),new T(function(){return E(E(_1mj)[2]);})]];},_1ml=function(_1mm){return E(E(_1mm)[2]);},_1mn=[0,0],_1mo=function(_1mp,_1mq){return new F(function(){return (function(_1mr,_1ms){while(1){var _1mt=(function(_1mu,_1mv){var _1mw=E(_1mu);if(!_1mw[0]){return E(_1mv);}else{_1mr=_1mw[2];_1ms=new T(function(){return B(A(new T(function(){return B(_1kt(_1mp));}),[_1mv,_1mw[1]]));});return null;}})(_1mr,_1ms);if(_1mt!=null){return _1mt;}}})(_1mq,new T(function(){return B(A(_SF,[_1mp,_1mn]));}));});},_1mx=[0,1],_1my=[0,E(_1mx),E(_Je)],_1mz=function(_1mA){var _1mB=E(_1mA);return _1mB[0]==0?E(_1lu):function(_1mC){return new F(function(){return _Sn(_R6,_nZ,_K,new T(function(){return B(A(new T(function(){var _1mD=B(_IP(_1mB,0))-1|0;if(0<=_1mD){var _1mE=function(_1mF){return [1,[0,[0,_1mF],_1my],new T(function(){if(_1mF!=_1mD){var _1mG=B(_1mE(_1mF+1|0));}else{var _1mG=[0];}var _1mH=_1mG;return _1mH;})];},_1mI=B(_1mE(0));if(!_1mI[0]){var _1mJ=E(_1lt);}else{var _1mK=E(_1mI[1]),_1mL=E(_1mI[2]);if(!_1mL[0]){var _1mM=function(_1mN,_){return [1,[0,_1mK[1],_1mN]];};}else{var _1mM=function(_1mO){return new F(function(){return _Sn(_R6,_nZ,_K,new T(function(){return B(_Sn(_R6,_nZ,_K,function(_){return [1,new T(function(){var _1mP=B(_1k8(_Sk,_1ln,new T(function(){var _1mQ=B(_1mo(_Wr,B(_8A(_1ml,_1mI))));return B(_IM(_1mQ[1],_1mQ[2]));}),_1mO));return [0,_1mP[1],_1mP[2]];})];},_1mi));}),function(_1mR,_){return [1,[0,new T(function(){var _1mS=B(_K5(function(_1mT){return new F(function(){return _Ws(E(_1mT)[2],new T(function(){return E(E(_1mR)[1]);}));});},new T(function(){var _1mU=B(_1la(_1lo,_1mK,_1mL));return [1,_1mU[1],_1mU[2]];})));if(!_1mS[0]){var _1mV=E(_1lm);}else{var _1mV=E(E(_1mS[1])[1]);}return _1mV;}),new T(function(){return E(E(_1mR)[2]);})]];});});};}var _1mW=_1mM,_1mJ=_1mW;}var _1mX=_1mJ;}else{var _1mX=E(_1lt);}var _1mY=_1mX,_1mZ=_1mY,_1n0=_1mZ;return _1n0;}),[_1mC]));}),function(_1n1,_){return [1,[0,new T(function(){var _1n2=E(E(_1n1)[1])[1],_1n3=[1,new T(function(){if(_1n2>=0){var _1n4=E(B(_FM(_1mB,_1n2))[1])==0?E(_Vw):E(_Vv);}else{var _1n4=E(_FJ);}var _1n5=_1n4;return _1n5;}),new T(function(){var _1n6=_1n2+1|0;return _1n6>=0?B(_To(_1n6,_1mB)):E(_1mB);})];if(_1n2>0){var _1n7=function(_1n8,_1n9){var _1na=E(_1n8);if(!_1na[0]){return E(_1n3);}else{var _1nb=_1na[1];return _1n9>1?[1,_1nb,new T(function(){return B(_1n7(_1na[2],_1n9-1|0));})]:[1,_1nb,_1n3];}},_1nc=B((function(_1nd,_1ne,_1nf){return _1nf>1?[1,_1nd,new T(function(){return B(_1n7(_1ne,_1nf-1|0));})]:[1,_1nd,_1n3];})(_1mB[1],_1mB[2],_1n2));}else{var _1nc=E(_1n3);}var _1ng=_1nc,_1nh=_1ng,_1ni=_1nh;return _1ni;}),new T(function(){return E(E(_1n1)[2]);})]];});});};},_1nj=function(_1nk,_1nl){return new F(function(){return _1mz(_1nl);});},_1nm=function(_1nn,_1no){while(1){var _1np=E(_1no);if(!_1np[0]){return 0;}else{var _1nq=B(A(_1nn,[_1np[1]]));_1no=_1np[2];continue;}}},_1nr=function(_1ns){var _1nt=E(_1ns);return 0;},_1nu=function(_1nv){return new F(function(){return _1nm(_1nr,_1nv);});},_1nw=new T(function(){return B(unCStr("FuncIndivid "));}),_1nx=function(_1ny,_1nz,_1nA){return _1ny<11?B(_2J(_1nw,new T(function(){return B(_2Z(_zb,_1nz,_1nA));},1))):[1,_5o,new T(function(){return B(_2J(_1nw,new T(function(){return B(_2Z(_zb,_1nz,[1,_5n,_1nA]));},1)));})];},_1nB=function(_1nC){return new F(function(){return _1nx(0,_1nC,_1g);});},_1nD=function(_1nE,_1nF){return new F(function(){return _1nx(0,_1nE,_1nF);});},_1nG=function(_1nH,_9E){return new F(function(){return _2Z(_1nD,_1nH,_9E);});},_1nI=function(_1nJ,_1nK,_1nL){return new F(function(){return _1nx(E(_1nJ)[1],_1nK,_1nL);});},_1nM=[0,_1nI,_1nB,_1nG],_1nN=[0,_1nM,_1nu,_Vp,_1nj,_VL],_1nO=function(_1nP){return E(E(_1nP)[2]);},_1nQ=function(_1nR){return E(E(_1nR)[5]);},_1nS=function(_1nT,_){return [1,[0,_1g,_1nT]];},_1nU=function(_1nV,_1nW,_1nX){return new F(function(){return _Sn(_R6,_nZ,_K,new T(function(){return B(A(_1nV,[_1nX]));}),function(_1nY){return new F(function(){return _Sn(_R6,_nZ,_K,new T(function(){return B(A(_1nW,[new T(function(){return E(E(_1nY)[2]);})]));}),function(_1nZ,_){return [1,[0,[1,new T(function(){return E(E(_1nY)[1]);}),new T(function(){return E(E(_1nZ)[1]);})],new T(function(){return E(E(_1nZ)[2]);})]];});});});});},_1o0=function(_1o1,_1o2,_1o3){if(_1o3>0){var _1o4=new T(function(){return B(A(_1nQ,[_1o1,_1o2]));}),_1o5=function(_1o6){return _1o6>1?function(_ay){return new F(function(){return _1nU(_1o4,new T(function(){return B(_1o5(_1o6-1|0));}),_ay);});}:function(_1o7){return new F(function(){return _1nU(_1o4,_1nS,_1o7);});};};return new F(function(){return _1o5(_1o3);});}else{return E(_1nS);}},_1o8=function(_1o9,_){var _1oa=E(_1o9);return [1,new T(function(){var _1ob=E(_1oa[1]);return [0,_1ob[1],_1ob[2],_1ob[3],_1ob[4],_1oa[2]];})];},_1oc=function(_1od,_1oe,_1of){while(1){var _1og=E(_1of);if(!_1og[0]){return [0,_1od,_1oe];}else{var _1oh=_1og[2],_1oi=E(_1og[1]),_1oj=_1oi[1],_1ok=_1oi[2];if(!B(_s5(B(_qP(_1od,_1ok)),B(_qP(_1oj,_1oe))))){_1of=_1oh;continue;}else{_1od=_1oj;_1oe=_1ok;_1of=_1oh;continue;}}}},_1ol=function(_1om){return E(E(_1om)[3]);},_1on=function(_1oo,_1op,_1oq,_1or,_1os){if(!B(_JM(_1or,_11D))){var _1ot=function(_1ou){if(!B(_4d(_1ou,_1os))){return new F(function(){return A(_1oo,[_1ou,new T(function(){return B(_1ot(B(_qx(_1ou,_1or))));})]);});}else{return E(_1op);}};return new F(function(){return _1ot(_1oq);});}else{var _1ov=function(_1ow){if(!B(_K9(_1ow,_1os))){return new F(function(){return A(_1oo,[_1ow,new T(function(){return B(_1ov(B(_qx(_1ow,_1or))));})]);});}else{return E(_1op);}};return new F(function(){return _1ov(_1oq);});}},_1ox=function(_1oy,_1oz){var _1oA=E(_1oz);return _1oA[0]==0?[0]:[1,_1oy,new T(function(){return B(_1ox(_1oA[1],_1oA[2]));})];},_1oB=new T(function(){return B(unCStr("init"));}),_1oC=new T(function(){return B(_1lj(_1oB));}),_1oD=function(_1oE,_1oF){var _1oG=E(E(_1oF)[1])[1],_1oH=E(E(_1oE)[1])[1];return _1oG>=_1oH?_1oG!=_1oH?2:1:0;},_1oI=[0,25],_1oJ=[0,0],_1oK=function(_1oL,_){return [1,[0,_c,_1oL]];},_1oM=new T(function(){return B(_3j(_1oI,_1oJ));}),_1oN=function(_1oO,_){return [1,[0,new T(function(){return B(_km(E(_1oO)[1]));}),new T(function(){return E(E(_1oO)[2]);})]];},_1oP=new T(function(){return B(unCStr("maximum"));}),_1oQ=new T(function(){return B(_1lj(_1oP));}),_1oR=function(_1oS){return E(E(_1oS)[4]);},_1oT=[1,_c],_1oU=function(_){return _1oT;},_1oV=[0,_c,_1oU],_1oW=[0,_1oV],_1oX=function(_){return _1oW;},_1oY=function(_1oZ){return new F(function(){return _Sn(_R6,_nZ,_K,_1oX,function(_1p0,_){return [1,[0,_1p0,_1oZ]];});});},_1p1=function(_1p2,_1p3,_1p4){return new F(function(){return _Sn(_R6,_nZ,_K,new T(function(){return B(A(_1p2,[_1p4]));}),function(_1p5){return new F(function(){return _Sn(_R6,_nZ,_K,new T(function(){return B(A(_1p3,[new T(function(){return E(E(_1p5)[2]);})]));}),function(_1p6,_){return [1,[0,[1,new T(function(){return E(E(_1p5)[1]);}),new T(function(){return E(E(_1p6)[1]);})],new T(function(){return E(E(_1p6)[2]);})]];});});});});},_1p7=function(_1p8,_){return [1,[0,_1g,_1p8]];},_1p9=function(_1pa){return [0];},_1pb=function(_1pc){var _1pd=E(_1pc);return new F(function(){return A(_1pd[1],[_1pd[2]]);});},_1pe=function(_1pf,_){return [1,[0,new T(function(){var _1pg=B(_1ma(E(E(_1pf)[1])[1]));return [0,E(_1pg[1]),E(_1pg[2])];}),new T(function(){return E(E(_1pf)[2]);})]];},_1ph=[0,1],_1pi=function(_1pj,_1pk,_1pl){var _1pm=[1,[0,_1pl,new T(function(){var _1pn=E(_1pj),_1po=B(_W0(_1ph,_Je,_1pn[1],_1pn[2]));return [0,E(_1po[1]),E(_1po[2])];})],_1g],_1pp=[0,_1pk,_1pj];return function(_1pq){return new F(function(){return _Sn(_R6,_nZ,_K,new T(function(){return B(_Sn(_R6,_nZ,_K,new T(function(){return B(_Sn(_R6,_nZ,_K,function(_){return [1,new T(function(){var _1pr=B(_1k8(_Sk,_1ln,new T(function(){var _1ps=B(_1mo(_Wr,B(_8A(_1ml,[1,_1pp,_1pm]))));return B(_IM(_1ps[1],_1ps[2]));}),_1pq));return [0,_1pr[1],_1pr[2]];})];},_1pe));}),function(_1pt,_){return [1,[0,new T(function(){var _1pu=B(_K5(function(_1pv){return new F(function(){return _Ws(E(_1pv)[2],new T(function(){return E(E(_1pt)[1]);}));});},new T(function(){var _1pw=B(_1la(_1lo,_1pp,_1pm));return [1,_1pw[1],_1pw[2]];})));if(!_1pu[0]){var _1px=E(_1lm);}else{var _1px=E(E(_1pu[1])[1]);}return _1px;}),new T(function(){return E(E(_1pt)[2]);})]];}));}),_1pb);});};},_1py=function(_1pz,_1pA,_1pB,_1pC,_1pD){var _1pE=new T(function(){return B(_1oR(_1pz));}),_1pF=new T(function(){var _1pG=B(_8A(_1ml,B(_TT(_1oD,B(_7H(function(_1pH,_1pI,_1pJ){return [1,[0,new T(function(){return B(A(_1pB,[_1pH]));}),_1pI],_1pJ];},_1g,_1pD,_1pD)))))),_1pK=B(_IP(_1pG,0))*E(E(_1pC)[2])[1],_1pL=_1pK&4294967295;if(_1pL>=_1pK){if(_1pL>0){var _1pM=_1pL<0?[0]:B(_Tt(_1pL,_1pG));}else{var _1pM=[0];}var _1pN=_1pM,_1pO=_1pN;}else{var _1pP=_1pL+1|0;if(_1pP>0){var _1pQ=_1pP<0?[0]:B(_Tt(_1pP,_1pG));}else{var _1pQ=[0];}var _1pR=_1pQ,_1pS=_1pR,_1pO=_1pS;}var _1pT=_1pO,_1pU=_1pT,_1pV=_1pU,_1pW=_1pV,_1pX=_1pW,_1pY=_1pX,_1pZ=_1pY,_1q0=_1pZ,_1q1=_1q0,_1q2=_1q1,_1q3=_1q2;return _1q3;});return function(_1q4){return new F(function(){return _Sn(_R6,_nZ,_K,new T(function(){return B(_Sn(_R6,_nZ,_K,new T(function(){return B(A(new T(function(){var _1q5=B(_1lx( -((B(_IP(_1pD,0))-B(_IP(_1pF,0))|0)/2))),_1q6=_1q5[1],_1q7=_1q5[2],_1q8=new T(function(){var _1q9=B(_1ma(E(E(_1pC)[1])[1]));return [0,E(_1q9[1]),E(_1q9[2])];}),_1qa=new T(function(){var _1qb=B(_8A(function(_1qc){var _1qd=B(_1ma(B(A(_1pB,[_1qc]))[1]));return [0,E(_1qd[1]),E(_1qd[2])];},_1pD)),_1qe=function(_1qf){var _1qg=E(_1qf);return _1qg[0]==0?E(_1p9):function(_1qh){var _1qi=E(_1qh);return _1qi[0]==0?[0]:[1,[0,_1qi[1],new T(function(){var _1qj=E(_1qg[1]),_1qk=E(new T(function(){var _1ql=E(_1qb);if(!_1ql[0]){var _1qm=E(_1oQ);}else{var _1qn=E(_1ql[1]),_1qo=B(_1oc(_1qn[1],_1qn[2],_1ql[2])),_1qm=[0,E(_1qo[1]),E(_1qo[2])];}return _1qm;})),_1qp=B(_111(_1qj[1],_1qj[2],_1qk[1],_1qk[2]));return [0,E(_1qp[1]),E(_1qp[2])];})],new T(function(){return B(A(new T(function(){return B(_1qe(_1qg[2]));}),[_1qi[2]]));})];};},_1qq=B(A(_1qe,[_1qb,_1pD]));if(!_1qq[0]){var _1qr=E(_1lt);}else{var _1qs=E(_1qq[1]),_1qt=E(_1qq[2]);if(!_1qt[0]){var _1qu=function(_1qv,_){return [1,[0,_1qs[1],_1qv]];};}else{var _1qu=function(_1qw){return new F(function(){return _Sn(_R6,_nZ,_K,new T(function(){return B(_Sn(_R6,_nZ,_K,function(_){return [1,new T(function(){var _1qx=B(_1k8(_Sk,_1ln,new T(function(){var _1qy=B(_1mo(_Wr,B(_8A(_1ml,_1qq))));return B(_IM(_1qy[1],_1qy[2]));}),_1qw));return [0,_1qx[1],_1qx[2]];})];},_1pe));}),function(_1qz,_){return [1,[0,new T(function(){var _1qA=B(_K5(function(_1qB){return new F(function(){return _Ws(E(_1qB)[2],new T(function(){return E(E(_1qz)[1]);}));});},new T(function(){var _1qC=B(_1la(_1lo,_1qs,_1qt));return [1,_1qC[1],_1qC[2]];})));if(!_1qA[0]){var _1qD=E(_1lm);}else{var _1qD=E(E(_1qA[1])[1]);}return _1qD;}),new T(function(){return E(E(_1qz)[2]);})]];});});};}var _1qE=_1qu,_1qr=_1qE;}var _1qF=_1qr;return _1qF;}),_1qG=function(_1qH){return function(_1qI){return new F(function(){return _Sn(_R6,_nZ,_K,new T(function(){return B(A(new T(function(){if(!E(_1oM)){var _1qJ=!B(_3j(B(_SL(_1qH,_1oI)),_1oJ))?E(_1oK):E(_1oY);}else{var _1qJ=E(_3i);}return _1qJ;}),[_1qI]));}),function(_1qK){return new F(function(){return _Sn(_R6,_nZ,_K,new T(function(){return B(A(_1qa,[new T(function(){return E(E(_1qK)[2]);})]));}),function(_1qL){return new F(function(){return _Sn(_R6,_nZ,_K,new T(function(){return B(A(_1qa,[new T(function(){return E(E(_1qL)[2]);})]));}),function(_1qM){return new F(function(){return _Sn(_R6,_nZ,_K,new T(function(){return B(A(new T(function(){return B(_1ol(_1pz));}),[_1pA,new T(function(){return E(E(_1qL)[1]);}),new T(function(){return E(E(_1qM)[1]);}),new T(function(){return E(E(_1qM)[2]);})]));}),function(_1qN){var _1qO=E(_1qN),_1qP=E(_1qO[1]),_1qQ=_1qP[1],_1qR=_1qP[2];return new F(function(){return _Sn(_R6,_nZ,_K,new T(function(){return B(_Sn(_R6,_nZ,_K,new T(function(){var _1qS=[1,[0,function(_1qT,_){return [1,[0,_1qQ,_1qT]];},new T(function(){var _1qU=E(_1q8),_1qV=B(_W0(_1ph,_Je,_1qU[1],_1qU[2]));return [0,E(_1qV[1]),E(_1qV[2])];})],_1g],_1qW=[0,new T(function(){return B(A(_1pE,[_1pA,_1qQ]));}),_1q8];return B(_Sn(_R6,_nZ,_K,new T(function(){return B(_Sn(_R6,_nZ,_K,function(_){return [1,new T(function(){var _1qX=B(_1k8(_Sk,_1ln,new T(function(){var _1qY=B(_1mo(_Wr,B(_8A(_1ml,[1,_1qW,_1qS]))));return B(_IM(_1qY[1],_1qY[2]));}),_1qO[2]));return [0,_1qX[1],_1qX[2]];})];},_1pe));}),function(_1qZ,_){return [1,[0,new T(function(){var _1r0=B(_K5(function(_1r1){return new F(function(){return _Ws(E(_1r1)[2],new T(function(){return E(E(_1qZ)[1]);}));});},new T(function(){var _1r2=B(_1la(_1lo,_1qW,_1qS));return [1,_1r2[1],_1r2[2]];})));if(!_1r0[0]){var _1r3=E(_1lm);}else{var _1r3=E(E(_1r0[1])[1]);}return _1r3;}),new T(function(){return E(E(_1qZ)[2]);})]];}));}),_1pb));}),function(_1r4){return new F(function(){return _Sn(_R6,_nZ,_K,new T(function(){return B(A(new T(function(){return B(_1pi(_1q8,new T(function(){return B(A(_1pE,[_1pA,_1qR]));}),function(_1r5,_){return [1,[0,_1qR,_1r5]];}));}),[new T(function(){return E(E(_1r4)[2]);})]));}),function(_1r6,_){return [1,[0,[1,new T(function(){return E(E(_1r4)[1]);}),[1,new T(function(){return E(E(_1r6)[1]);}),_1g]],new T(function(){return E(E(_1r6)[2]);})]];});});});});});});});});});});});});};};if(_1q7>=0){var _1r7=B(_1on(function(_1r8,_1r9){return function(_ay){return new F(function(){return _1p1(new T(function(){return B(_1qG(_1r8));}),_1r9,_ay);});};},_1p7,_1ph,_1ph,B(_qH(B(_1m6(_1q6,_1q7))))));}else{var _1ra= -_1q7;if(_1ra<=52){var _1rb=hs_uncheckedIShiftRA64(B(_Y7(_1q6)),_1ra),_1rc=_1rb,_1rd=B(_1on(function(_1re,_1rf){return function(_ay){return new F(function(){return _1p1(new T(function(){return B(_1qG(_1re));}),_1rf,_ay);});};},_1p7,_1ph,_1ph,B(_qH(B(_Yy(_1rc))))));}else{if(!B(_4d(_1q6,_1oJ))){var _1rg=B(_1on(function(_1rh,_1ri){return function(_ay){return new F(function(){return _1p1(new T(function(){return B(_1qG(_1rh));}),_1ri,_ay);});};},_1p7,_1ph,_1ph,_1oJ));}else{var _1rg=B(_1on(function(_1rj,_1rk){return function(_ay){return new F(function(){return _1p1(new T(function(){return B(_1qG(_1rj));}),_1rk,_ay);});};},_1p7,_1ph,_1ph,_1ph));}var _1rd=_1rg;}var _1rl=_1rd,_1rm=_1rl,_1r7=_1rm;}var _1rn=_1r7,_1ro=_1rn,_1rp=_1ro,_1rq=_1rp,_1rr=_1rq,_1rs=_1rr,_1rt=_1rs,_1ru=_1rt;return _1ru;}),[_1q4]));}),_1oN));}),function(_1rv,_){return [1,[0,new T(function(){var _1rw=B(_2J(_1pF,new T(function(){return E(E(_1rv)[1]);},1)));if(B(_IP(_1rw,0))>E(new T(function(){return [0,B(_IP(_1pD,0))];}))[1]){var _1rx=E(_1rw),_1ry=_1rx[0]==0?E(_1oC):B(_1ox(_1rx[1],_1rx[2]));}else{var _1ry=E(_1rw);}var _1rz=_1ry,_1rA=_1rz,_1rB=_1rA,_1rC=_1rB;return _1rC;}),new T(function(){return E(E(_1rv)[2]);})]];});});};},_1rD=function(_1rE,_1rF,_1rG){return new F(function(){return _Sn(_R6,_nZ,_K,new T(function(){return B(A(_1rE,[_1rG]));}),function(_1rH){return new F(function(){return _Sn(_R6,_nZ,_K,new T(function(){return B(A(_1rF,[new T(function(){return E(E(_1rH)[2]);})]));}),function(_1rI,_){return [1,[0,[1,new T(function(){return E(E(_1rH)[1]);}),new T(function(){return E(E(_1rI)[1]);})],new T(function(){return E(E(_1rI)[2]);})]];});});});});},_1rJ=function(_1rK,_){return [1,[0,_1g,_1rK]];},_1rL=function(_1rM,_1rN,_1rO,_1rP,_1rQ,_1rR,_1rS,_1rT,_1rU,_){return !E(_1rQ)?B(A(_Sn,[_R6,_nZ,_K,new T(function(){var _1rV=function(_1rW){var _1rX=E(_1rW);return _1rX[0]==0?E(_1rJ):function(_1rY){return new F(function(){return _Sn(_R6,_nZ,_K,new T(function(){return B(_Sn(_R6,_nZ,_K,new T(function(){return B(_1oY(_1rY));}),function(_1rZ){return new F(function(){return A(new T(function(){return B(_1py(_1rM,_1rN,_1rO,_1rP,_1rX[1]));}),[new T(function(){return E(E(_1rZ)[2]);})]);});}));}),function(_1s0){return new F(function(){return _Sn(_R6,_nZ,_K,new T(function(){return B(A(new T(function(){return B(_1rV(_1rX[2]));}),[new T(function(){return E(E(_1s0)[2]);})]));}),function(_1s1,_){return [1,[0,[1,new T(function(){return E(E(_1s0)[1]);}),new T(function(){return E(E(_1s1)[1]);})],new T(function(){return E(E(_1s1)[2]);})]];});});});});};};return B(_Sn(_R6,_nZ,_K,new T(function(){if(!E(E(_1rR)[1])){var _1s2=E(_1rP),_1s3=E(_1s2[4])[1];if(_1s3>0){var _1s4=function(_1s5){return new F(function(){return _Sn(_R6,_nZ,_K,new T(function(){return B(_1oY(_1s5));}),function(_1s6){return new F(function(){return A(new T(function(){return B(_1o0(_1rM,_1rN,E(_1s2[5])[1]));}),[new T(function(){return E(E(_1s6)[2]);})]);});});});},_1s7=function(_1s8){return _1s8>1?function(_ay){return new F(function(){return _1rD(_1s4,new T(function(){return B(_1s7(_1s8-1|0));}),_ay);});}:function(_1s9){return new F(function(){return _1rD(_1s4,_1rJ,_1s9);});};},_1sa=B(A(_1s7,[_1s3,_1rU]));}else{var _1sa=function(_){return [1,[0,_1g,_1rU]];};}var _1sb=_1sa,_1sc=_1sb,_1sd=_1sc,_1se=_1sd;}else{var _1se=function(_){return [1,[0,_1rS,_1rU]];};}var _1sf=_1se;return _1sf;}),function(_1sg){return new F(function(){return _Sn(_R6,_nZ,_K,new T(function(){var _1sh=new T(function(){return E(E(_1sg)[1]);}),_1si=B(_1nm(function(_1s9){return new F(function(){return _1nm(new T(function(){return B(_1nO(_1rM));}),_1s9);});},_1sh));return B(A(_1rV,[_1sh,new T(function(){return E(E(_1sg)[2]);})]));}),function(_1sj,_){var _1sk=new T(function(){return E(E(_1sj)[1]);}),_1sl=new T(function(){return B(_8E(_1rO,_1sk));});return [1,[0,[0,new T(function(){var _1sm=E(_1sl);if(!_1sm[0]){var _1sn=false;}else{if(!E(new T(function(){return (E(_1rR)[1]+1|0)>=E(E(_1rP)[3])[1];}))){var _1so=E(E(_1rP)[6]);if(!_1so[0]){var _1sp=false;}else{var _1sp=E(E(_1sm[1])[1])[1]>=E(_1so[1])[1];}var _1sq=_1sp,_1sr=_1sq;}else{var _1sr=true;}var _1sn=_1sr;}return _1sn;}),new T(function(){return [0,E(_1rR)[1]+1|0];}),_1sk,_1sl,_1rU],new T(function(){return E(E(_1sj)[2]);})]];});});}));}),_1o8,_])):[1,[0,_gw,_1rR,_1rS,_1rT,_1rU]];},_1ss=function(_1st,_1su,_1sv){while(1){var _1sw=E(_1sv);if(!_1sw[0]){return [0,_1st,[0,_1su]];}else{var _1sx=_1sw[2],_1sy=E(_1sw[1]),_1sz=E(_1sy[2])[1];if(_1su>=_1sz){if(_1su!=_1sz){_1st=_1sy[1];_1su=_1sz;_1sv=_1sx;continue;}else{_1sv=_1sx;continue;}}else{_1sv=_1sx;continue;}}}},_1sA=function(_1sB,_1sC,_1sD){while(1){var _1sE=E(_1sD);if(!_1sE[0]){return [0,_1sB,[0,_1sC]];}else{var _1sF=_1sE[2],_1sG=E(_1sE[1]),_1sH=_1sG[1],_1sI=E(_1sG[2])[1];if(_1sC>=_1sI){if(_1sC!=_1sI){_1sD=_1sF;continue;}else{_1sB=_1sH;_1sC=_1sI;_1sD=_1sF;continue;}}else{_1sB=_1sH;_1sC=_1sI;_1sD=_1sF;continue;}}}},_1sJ=function(_1sK,_1sL,_1sM){while(1){var _1sN=E(_1sM);if(!_1sN[0]){return [0,[0,_1sK],_1sL];}else{var _1sO=_1sN[2],_1sP=E(_1sN[1]),_1sQ=E(_1sP[1])[1];if(_1sK>=_1sQ){if(_1sK!=_1sQ){_1sK=_1sQ;_1sL=_1sP[2];_1sM=_1sO;continue;}else{_1sM=_1sO;continue;}}else{_1sM=_1sO;continue;}}}},_1sR=function(_1sS,_1sT,_1sU){while(1){var _1sV=E(_1sU);if(!_1sV[0]){return [0,[0,_1sS],_1sT];}else{var _1sW=_1sV[2],_1sX=E(_1sV[1]),_1sY=_1sX[2],_1sZ=E(_1sX[1])[1];if(_1sS>=_1sZ){if(_1sS!=_1sZ){_1sU=_1sW;continue;}else{_1sS=_1sZ;_1sT=_1sY;_1sU=_1sW;continue;}}else{_1sS=_1sZ;_1sT=_1sY;_1sU=_1sW;continue;}}}},_1t0=function(_1t1,_1t2,_1t3,_1t4,_){var _1t5=jsMoveTo(_1t4,_1t1+_1t3,_1t2),_1t6=jsArc(_1t4,_1t1,_1t2,_1t3,0,6.283185307179586);return _c;},_1t7=function(_1t8,_1t9,_){var _1ta=jsBeginPath(_1t9),_1tb=B(A(_1t8,[[0,_1t9],_])),_1tc=_1tb,_1td=jsFill(_1t9);return _c;},_1te=function(_1tf,_1tg){var _1th=_1tf-1|0;if(_1th>=0){var _1ti=B(_To(_1th,_1tg));return _1ti[0]==0?[0]:[1,_1ti[1],new T(function(){return B(_1te(_1tf,_1ti[2]));})];}else{var _1tj=E(_1tg);return _1tj[0]==0?[0]:[1,_1tj[1],new T(function(){return B(_1te(_1tf,_1tj[2]));})];}},_1tk=new T(function(){return B(unCStr("n is larger than passed list!"));}),_1tl=new T(function(){return B(err(_1tk));}),_1tm=function(_1tn,_1to){var _1tp=B(_IP(_1to,0));return _1tn<=_1tp?_1tn>0?_1tn<0?[0]:B(_Tt(_1tn,new T(function(){var _1tq=jsRound(_1tp/_1tn),_1tr=_1tq;return B(_1te(_1tr,_1to));},1))):[0]:E(_1tl);},_1ts=function(_1tt,_){return _c;},_1tu=function(_1tv,_){return _1g;},_1tw=new T(function(){return [0,toJSStr(_1g)];}),_1tx=new T(function(){return [0,"rgb("];}),_1ty=[0,44],_1tz=[1,_1ty,_1g],_1tA=new T(function(){return [0,toJSStr(_1tz)];}),_1tB=new T(function(){return [0,"rgba("];}),_1tC=[0,41],_1tD=[1,_1tC,_1g],_1tE=new T(function(){return [0,toJSStr(_1tD)];}),_1tF=[1,_1tE,_1g],_1tG=function(_1tH){var _1tI=E(_1tH);if(!_1tI[0]){var _1tJ=jsCat([1,_1tx,[1,new T(function(){var _1tK=String(_1tI[1]),_1tL=_1tK;return [0,_1tL];}),[1,_1tA,[1,new T(function(){var _1tM=String(_1tI[2]),_1tN=_1tM;return [0,_1tN];}),[1,_1tA,[1,new T(function(){var _1tO=String(_1tI[3]),_1tP=_1tO;return [0,_1tP];}),_1tF]]]]]],E(_1tw)[1]),_1tQ=_1tJ;return E(_1tQ);}else{var _1tR=jsCat([1,_1tB,[1,new T(function(){var _1tS=String(_1tI[1]),_1tT=_1tS;return [0,_1tT];}),[1,_1tA,[1,new T(function(){var _1tU=String(_1tI[2]),_1tV=_1tU;return [0,_1tV];}),[1,_1tA,[1,new T(function(){var _1tW=String(_1tI[3]),_1tX=_1tW;return [0,_1tX];}),[1,_1tA,[1,new T(function(){var _1tY=String(_1tI[4]),_1tZ=_1tY;return [0,_1tZ];}),_1tF]]]]]]]],E(_1tw)[1]),_1u0=_1tR;return E(_1u0);}},_1u1=new T(function(){return [0,"strokeStyle"];}),_1u2=new T(function(){return [0,"fillStyle"];}),_1u3=function(_1u4,_1u5){return function(_1u6,_){var _1u7=E(_1u6),_1u8=_1u7[1],_1u9=E(_1u2)[1],_1ua=jsGet(_1u8,_1u9),_1ub=_1ua,_1uc=E(_1u1)[1],_1ud=jsGet(_1u8,_1uc),_1ue=_1ud,_1uf=E(new T(function(){return [0,B(_1tG(_1u4))];}))[1],_1ug=jsSet(_1u8,_1u9,_1uf),_1uh=jsSet(_1u8,_1uc,_1uf),_1ui=B(A(_1u5,[_1u7,_])),_1uj=_1ui,_1uk=jsSet(_1u8,_1u9,_1ub),_1ul=jsSet(_1u8,_1uc,_1ue);return _c;};},_1um=function(_1un,_){return _c;},_1uo=function(_1up){var _1uq=E(_1up);if(!_1uq[0]){return E(_1um);}else{var _1ur=E(_1uq[1]);return function(_1us,_){var _1ut=E(_1us)[1],_1uu=jsMoveTo(_1ut,E(_1ur[1])[1],E(_1ur[2])[1]);return new F(function(){return (function(_1uv,_){while(1){var _1uw=E(_1uv);if(!_1uw[0]){return _c;}else{var _1ux=E(_1uw[1]),_1uy=jsLineTo(_1ut,E(_1ux[1])[1],E(_1ux[2])[1]);_1uv=_1uw[2];continue;}}})(_1uq[2],_);});};}},_1uz=function(_1uA){var _1uB=E(_1uA);if(!_1uB[0]){return E(_1ts);}else{var _1uC=_1uB[1];return function(_1uD,_){var _1uE=E(_1uD),_1uF=_1uE[1],_1uG=jsBeginPath(_1uF),_1uH=B(A(new T(function(){return B(_1uo([1,new T(function(){return E(E(_1uC)[1]);}),[1,new T(function(){return E(E(_1uC)[2]);}),_1g]]));}),[[0,_1uF],_])),_1uI=_1uH,_1uJ=jsStroke(_1uF);return new F(function(){return A(new T(function(){return B(_1uz(_1uB[2]));}),[_1uE,_]);});};}},_1uK=function(_1uL){var _1uM=E(_1uL);if(!_1uM[0]){return E(_1ts);}else{var _1uN=_1uM[1];return function(_1uO,_){var _1uP=E(_1uO),_1uQ=_1uP[1],_1uR=jsBeginPath(_1uQ),_1uS=B(A(new T(function(){return B(_1uo([1,new T(function(){return E(E(_1uN)[1]);}),[1,new T(function(){return E(E(_1uN)[2]);}),_1g]]));}),[[0,_1uQ],_])),_1uT=_1uS,_1uU=jsStroke(_1uQ);return new F(function(){return A(new T(function(){return B(_1uK(_1uM[2]));}),[_1uP,_]);});};}},_1uV=function(_1uW,_1uX){while(1){var _1uY=E(_1uX);if(!_1uY[0]){return E(_1uW);}else{_1uW=_1uY[1];_1uX=_1uY[2];continue;}}},_1uZ=new T(function(){return B(unCStr("last"));}),_1v0=new T(function(){return B(_1lj(_1uZ));}),_1v1=[0,2],_1v2=[0,0],_1v3=[0,_1v2,_1v2],_1v4=[1,_1v3,_1g],_1v5=[0,1],_1v6=[0,125,125,125],_1v7=[0,0],_1v8=new T(function(){return B(unCStr("List.minimumBy: empty list"));}),_1v9=new T(function(){return B(err(_1v8));}),_1va=new T(function(){return B(unCStr("tail"));}),_1vb=new T(function(){return B(_1lj(_1va));}),_1vc=function(_1vd){return E(_1vd);},_1ve=[0,102],_1vf=[1,_1ve,_1g],_1vg=function(_1vh,_1vi){while(1){var _1vj=E(_1vh);if(!_1vj[0]){return E(_1vi);}else{_1vh=_1vj[2];var _1vk=[1,_1vj[1],_1vi];_1vi=_1vk;continue;}}},_1vl=function(_1vm){var _1vn=E(_1vm)[1];return [0,Math.log(_1vn+(_1vn+1)*Math.sqrt((_1vn-1)/(_1vn+1)))];},_1vo=function(_1vp){var _1vq=E(_1vp)[1];return [0,Math.log(_1vq+Math.sqrt(1+_1vq*_1vq))];},_1vr=function(_1vs){var _1vt=E(_1vs)[1];return [0,0.5*Math.log((1+_1vt)/(1-_1vt))];},_1vu=function(_1vv,_1vw){return [0,Math.log(E(_1vw)[1])/Math.log(E(_1vv)[1])];},_1vx=[0,3.141592653589793],_1vy=function(_1vz){return [0,Math.acos(E(_1vz)[1])];},_1vA=function(_1vB){return [0,Math.asin(E(_1vB)[1])];},_1vC=function(_1vD){return [0,Math.atan(E(_1vD)[1])];},_1vE=function(_1vF){return [0,Math.cos(E(_1vF)[1])];},_1vG=function(_1vH){return [0,cosh(E(_1vH)[1])];},_1vI=function(_1vJ){return [0,Math.exp(E(_1vJ)[1])];},_1vK=function(_1vL){return [0,Math.log(E(_1vL)[1])];},_1vM=function(_1vN,_1vO){return [0,Math.pow(E(_1vN)[1],E(_1vO)[1])];},_1vP=function(_1vQ){return [0,Math.sin(E(_1vQ)[1])];},_1vR=function(_1vS){return [0,sinh(E(_1vS)[1])];},_1vT=function(_1vU){return [0,Math.sqrt(E(_1vU)[1])];},_1vV=function(_1vW){return [0,Math.tan(E(_1vW)[1])];},_1vX=function(_1vY){return [0,tanh(E(_1vY)[1])];},_1vZ=[0,_X6,_1vx,_1vI,_1vT,_1vK,_1vM,_1vu,_1vP,_1vV,_1vE,_1vA,_1vC,_1vy,_1vR,_1vX,_1vG,_1vo,_1vr,_1vl],_1w0=function(_1w1){var _1w2=E(_1w1)[1];return [0,Math.log(_1w2+(_1w2+1)*Math.sqrt((_1w2-1)/(_1w2+1)))];},_1w3=function(_1w4){var _1w5=E(_1w4)[1];return [0,Math.log(_1w5+Math.sqrt(1+_1w5*_1w5))];},_1w6=function(_1w7){var _1w8=E(_1w7)[1];return [0,0.5*Math.log((1+_1w8)/(1-_1w8))];},_1w9=function(_1wa,_1wb){return [0,Math.log(E(_1wb)[1])/Math.log(E(_1wa)[1])];},_1wc=[0,3.141592653589793],_1wd=new T(function(){return [0,0/0];}),_1we=new T(function(){return [0,-1/0];}),_1wf=new T(function(){return [0,1/0];}),_1wg=function(_1wh,_1wi){return !B(_3j(_1wi,_IG))?[0,B(_IH(_1wh,_1wi))]:!B(_3j(_1wh,_IG))?!B(_4d(_1wh,_IG))?E(_1wf):E(_1we):E(_1wd);},_1wj=function(_1wk){var _1wl=E(_1wk);return new F(function(){return _1wg(_1wl[1],_1wl[2]);});},_1wm=function(_1wn){return [0,1/E(_1wn)[1]];},_1wo=function(_1wp){var _1wq=E(_1wp),_1wr=_1wq[1];return _1wr<0?[0, -_1wr]:E(_1wq);},_1ws=function(_1wt){var _1wu=E(_1wt);return _1wu[0]==0?_1wu[1]:I_toNumber(_1wu[1]);},_1wv=function(_1ww){return [0,B(_1ws(_1ww))];},_1wx=[0,0],_1wy=[0,1],_1wz=[0,-1],_1wA=function(_1wB){var _1wC=E(E(_1wB)[1]);return _1wC==0?E(_1wx):_1wC<=0?E(_1wz):E(_1wy);},_1wD=function(_1wE,_1wF){return [0,E(_1wE)[1]-E(_1wF)[1]];},_1wG=function(_1wH){return [0, -E(_1wH)[1]];},_1wI=function(_1wJ,_1wK){return [0,E(_1wJ)[1]+E(_1wK)[1]];},_1wL=function(_1wM,_1wN){return [0,E(_1wM)[1]*E(_1wN)[1]];},_1wO=[0,_1wI,_1wL,_1wD,_1wG,_1wo,_1wA,_1wv],_1wP=function(_1wQ,_1wR){return [0,E(_1wQ)[1]/E(_1wR)[1]];},_1wS=[0,_1wO,_1wP,_1wm,_1wj],_1wT=function(_1wU){return [0,Math.acos(E(_1wU)[1])];},_1wV=function(_1wW){return [0,Math.asin(E(_1wW)[1])];},_1wX=function(_1wY){return [0,Math.atan(E(_1wY)[1])];},_1wZ=function(_1x0){return [0,Math.cos(E(_1x0)[1])];},_1x1=function(_1x2){return [0,cosh(E(_1x2)[1])];},_1x3=function(_1x4){return [0,Math.exp(E(_1x4)[1])];},_1x5=function(_1x6){return [0,Math.log(E(_1x6)[1])];},_1x7=function(_1x8,_1x9){return [0,Math.pow(E(_1x8)[1],E(_1x9)[1])];},_1xa=function(_1xb){return [0,Math.sin(E(_1xb)[1])];},_1xc=function(_1xd){return [0,sinh(E(_1xd)[1])];},_1xe=function(_1xf){return [0,Math.sqrt(E(_1xf)[1])];},_1xg=function(_1xh){return [0,Math.tan(E(_1xh)[1])];},_1xi=function(_1xj){return [0,tanh(E(_1xj)[1])];},_1xk=[0,_1wS,_1wc,_1x3,_1xe,_1x5,_1x7,_1w9,_1xa,_1xg,_1wZ,_1wV,_1wX,_1wT,_1xc,_1xi,_1x1,_1w3,_1w6,_1w0],_1xl=function(_1xm){var _1xn=B(_1lx(E(_1xm)[1]));return [0,_1xn[1],[0,_1xn[2]]];},_1xo=[0,53],_1xp=function(_1xq){return E(_1xo);},_1xr=[0,2],_1xs=function(_1xt){return E(_1xr);},_1xu=[0,_IC,_IB],_1xv=function(_1xw){return E(_1xu);},_1xx=function(_1xy){var _1xz=isDoubleInfinite(E(_1xy)[1]),_1xA=_1xz;return E(_1xA)==0?false:true;},_1xB=function(_1xC){var _1xD=isDoubleNaN(E(_1xC)[1]),_1xE=_1xD;return E(_1xE)==0?false:true;},_1xF=function(_1xG){var _1xH=isDoubleNegativeZero(E(_1xG)[1]),_1xI=_1xH;return E(_1xI)==0?false:true;},_1xJ=function(_1xK){var _1xL=decodeFloat(E(_1xK)[1]);return [0,new T(function(){return B(_qN(_1xL[1]));}),[0,_1xL[2]]];},_1xM=[0,24],_1xN=function(_1xO){return E(_1xM);},_1xP=function(_1xQ){return E(_1xr);},_1xR=[0,128],_1xS=[0,-125],_1xT=[0,_1xS,_1xR],_1xU=function(_1xV){return E(_1xT);},_1xW=function(_1xX){var _1xY=isFloatInfinite(E(_1xX)[1]),_1xZ=_1xY;return E(_1xZ)==0?false:true;},_1y0=function(_1y1){var _1y2=isFloatNaN(E(_1y1)[1]),_1y3=_1y2;return E(_1y3)==0?false:true;},_1y4=function(_1y5){var _1y6=isFloatNegativeZero(E(_1y5)[1]),_1y7=_1y6;return E(_1y7)==0?false:true;},_1y8=function(_1y9){var _1ya=B(_1ma(E(_1y9)[1]));return [0,E(_1ya[1]),E(_1ya[2])];},_1yb=[0,_X2,_XF,_1y8],_1yc=function(_1yd,_1ye){if(_1yd<=_1ye){var _1yf=function(_1yg){return [1,[0,_1yg],new T(function(){if(_1yg!=_1ye){var _1yh=B(_1yf(_1yg+1|0));}else{var _1yh=[0];}var _1yi=_1yh;return _1yi;})];};return new F(function(){return _1yf(_1yd);});}else{return [0];}},_1yj=function(_1yk){return new F(function(){return _1yc(E(_1yk)[1],2147483647);});},_1yl=function(_1ym,_1yn,_1yo){return _1yo<=_1yn?[1,[0,_1ym],new T(function(){var _1yp=_1yn-_1ym|0,_1yq=function(_1yr){return _1yr>=(_1yo-_1yp|0)?[1,[0,_1yr],new T(function(){return B(_1yq(_1yr+_1yp|0));})]:[1,[0,_1yr],_1g];};return B(_1yq(_1yn));})]:_1yo<=_1ym?[1,[0,_1ym],_1g]:[0];},_1ys=function(_1yt,_1yu,_1yv){return _1yv>=_1yu?[1,[0,_1yt],new T(function(){var _1yw=_1yu-_1yt|0,_1yx=function(_1yy){return _1yy<=(_1yv-_1yw|0)?[1,[0,_1yy],new T(function(){return B(_1yx(_1yy+_1yw|0));})]:[1,[0,_1yy],_1g];};return B(_1yx(_1yu));})]:_1yv>=_1yt?[1,[0,_1yt],_1g]:[0];},_1yz=function(_1yA,_1yB){return _1yB<_1yA?B(_1yl(_1yA,_1yB,-2147483648)):B(_1ys(_1yA,_1yB,2147483647));},_1yC=function(_1yD,_1yE){return new F(function(){return _1yz(E(_1yD)[1],E(_1yE)[1]);});},_1yF=function(_1yG,_1yH,_1yI){return _1yH<_1yG?B(_1yl(_1yG,_1yH,_1yI)):B(_1ys(_1yG,_1yH,_1yI));},_1yJ=function(_1yK,_1yL,_1yM){return new F(function(){return _1yF(E(_1yK)[1],E(_1yL)[1],E(_1yM)[1]);});},_1yN=function(_1yO,_1yP){return new F(function(){return _1yc(E(_1yO)[1],E(_1yP)[1]);});},_1yQ=function(_1yR){return E(_1yR);},_1yS=new T(function(){return B(unCStr("Prelude.Enum.pred{Int}: tried to take `pred\' of minBound"));}),_1yT=new T(function(){return B(err(_1yS));}),_1yU=function(_1yV){var _1yW=E(E(_1yV)[1]);return _1yW==(-2147483648)?E(_1yT):[0,_1yW-1|0];},_1yX=new T(function(){return B(unCStr("Prelude.Enum.succ{Int}: tried to take `succ\' of maxBound"));}),_1yY=new T(function(){return B(err(_1yX));}),_1yZ=function(_1z0){var _1z1=E(E(_1z0)[1]);return _1z1==2147483647?E(_1yY):[0,_1z1+1|0];},_1z2=[0,_1yZ,_1yU,_1yQ,_1yQ,_1yj,_1yC,_1yN,_1yJ],_1z3=function(_1z4,_1z5){var _1z6=E(_1z5);switch(_1z6){case -1:var _1z7=E(_1z4);return _1z7==(-2147483648)?E(_YR):B(_122(_1z7,-1));case 0:return E(_3i);default:return new F(function(){return _122(_1z4,_1z6);});}},_1z8=function(_1z9,_1za){return [0,B(_1z3(E(_1z9)[1],E(_1za)[1]))];},_1zb=[0,0],_1zc=[0,_YR,_1zb],_1zd=function(_1ze,_1zf){var _1zg=E(_1ze)[1],_1zh=E(E(_1zf)[1]);switch(_1zh){case -1:var _1zi=E(_1zg);if(_1zi==(-2147483648)){return E(_1zc);}else{if(_1zi<=0){if(_1zi>=0){var _1zj=quotRemI(_1zi,-1);return [0,[0,_1zj[1]],[0,_1zj[2]]];}else{var _1zk=quotRemI(_1zi,-1);return [0,[0,_1zk[1]],[0,_1zk[2]]];}}else{var _1zl=quotRemI(_1zi-1|0,-1);return [0,[0,_1zl[1]-1|0],[0,(_1zl[2]+(-1)|0)+1|0]];}}break;case 0:return E(_3i);default:if(_1zg<=0){if(_1zg>=0){var _1zm=quotRemI(_1zg,_1zh);return [0,[0,_1zm[1]],[0,_1zm[2]]];}else{if(_1zh<=0){var _1zn=quotRemI(_1zg,_1zh);return [0,[0,_1zn[1]],[0,_1zn[2]]];}else{var _1zo=quotRemI(_1zg+1|0,_1zh);return [0,[0,_1zo[1]-1|0],[0,(_1zo[2]+_1zh|0)-1|0]];}}}else{if(_1zh>=0){if(_1zg>=0){var _1zp=quotRemI(_1zg,_1zh);return [0,[0,_1zp[1]],[0,_1zp[2]]];}else{if(_1zh<=0){var _1zq=quotRemI(_1zg,_1zh);return [0,[0,_1zq[1]],[0,_1zq[2]]];}else{var _1zr=quotRemI(_1zg+1|0,_1zh);return [0,[0,_1zr[1]-1|0],[0,(_1zr[2]+_1zh|0)-1|0]];}}}else{var _1zs=quotRemI(_1zg-1|0,_1zh);return [0,[0,_1zs[1]-1|0],[0,(_1zs[2]+_1zh|0)+1|0]];}}}},_1zt=function(_1zu,_1zv){var _1zw=E(E(_1zv)[1]);switch(_1zw){case -1:return E(_1zb);case 0:return E(_3i);default:return [0,B(_RN(E(_1zu)[1],_1zw))];}},_1zx=function(_1zy,_1zz){var _1zA=E(_1zy)[1],_1zB=E(E(_1zz)[1]);switch(_1zB){case -1:var _1zC=E(_1zA);return _1zC==(-2147483648)?E(_YR):[0,quot(_1zC,-1)];case 0:return E(_3i);default:return [0,quot(_1zA,_1zB)];}},_1zD=function(_1zE,_1zF){var _1zG=E(_1zE)[1],_1zH=E(E(_1zF)[1]);switch(_1zH){case -1:var _1zI=E(_1zG);if(_1zI==(-2147483648)){return E(_1zc);}else{var _1zJ=quotRemI(_1zI,-1);return [0,[0,_1zJ[1]],[0,_1zJ[2]]];}break;case 0:return E(_3i);default:var _1zK=quotRemI(_1zG,_1zH);return [0,[0,_1zK[1]],[0,_1zK[2]]];}},_1zL=function(_1zM,_1zN){var _1zO=E(E(_1zN)[1]);switch(_1zO){case -1:return E(_1zb);case 0:return E(_3i);default:return [0,E(_1zM)[1]%_1zO];}},_1zP=function(_1zQ){return new F(function(){return _qN(E(_1zQ)[1]);});},_1zR=function(_1zS){return [0,E(B(_qN(E(_1zS)[1]))),E(_Je)];},_1zT=function(_1zU,_1zV){return E(_1zU)[1]!=E(_1zV)[1];},_1zW=[0,_Ki,_1zT],_1zX=function(_1zY,_1zZ){var _1A0=E(_1zY),_1A1=E(_1zZ);return _1A0[1]>_1A1[1]?E(_1A0):E(_1A1);},_1A2=function(_1A3,_1A4){var _1A5=E(_1A3),_1A6=E(_1A4);return _1A5[1]>_1A6[1]?E(_1A6):E(_1A5);},_1A7=function(_1A8,_1A9){return E(_1A8)[1]>=E(_1A9)[1];},_1Aa=function(_1Ab,_1Ac){return E(_1Ab)[1]>E(_1Ac)[1];},_1Ad=function(_1Ae,_1Af){return E(_1Ae)[1]<=E(_1Af)[1];},_1Ag=function(_1Ah,_1Ai){return E(_1Ah)[1]<E(_1Ai)[1];},_1Aj=[0,_1zW,_TP,_1Ag,_1A7,_1Aa,_1Ad,_1zX,_1A2],_1Ak=[0,_Rw,_1Aj,_1zR],_1Al=[0,_1Ak,_1z2,_1zx,_1zL,_1z8,_1zt,_1zD,_1zd,_1zP],_1Am=function(_1An,_1Ao,_1Ap){while(1){if(!(_1Ao%2)){var _1Aq=B(_qP(_1An,_1An)),_1Ar=quot(_1Ao,2);_1An=_1Aq;_1Ao=_1Ar;continue;}else{var _1As=E(_1Ao);if(_1As==1){return new F(function(){return _qP(_1An,_1Ap);});}else{var _1Aq=B(_qP(_1An,_1An));_1Ao=quot(_1As-1|0,2);var _1At=B(_qP(_1An,_1Ap));_1An=_1Aq;_1Ap=_1At;continue;}}}},_1Au=function(_1Av,_1Aw){while(1){if(!(_1Aw%2)){var _1Ax=B(_qP(_1Av,_1Av)),_1Ay=quot(_1Aw,2);_1Av=_1Ax;_1Aw=_1Ay;continue;}else{var _1Az=E(_1Aw);if(_1Az==1){return E(_1Av);}else{return new F(function(){return _1Am(B(_qP(_1Av,_1Av)),quot(_1Az-1|0,2),_1Av);});}}}},_1AA=function(_1AB){return E(E(_1AB)[1]);},_1AC=function(_1AD){return E(E(_1AD)[2]);},_1AE=function(_1AF,_1AG,_1AH,_1AI,_1AJ){return new F(function(){return A(E(E(_1AG)[1])[1],[new T(function(){return B(A(_1AI,[_1AJ,new T(function(){return B(A(_SF,[_1AF,_3s]));})]));}),new T(function(){return B(A(_SF,[_1AF,_3r]));})]);});},_1AK=new T(function(){return B(err(_Jf));}),_1AL=function(_1AM,_1AN,_1AO,_1AP){var _1AQ=B(_13r(_1AN)),_1AR=_1AQ[1],_1AS=E(_1AQ[2]);if(!B(A(_1AS[3],[_1AP,new T(function(){return B(A(_SF,[_1AR,_3r]));})]))){if(!B(A(E(_1AS[1])[1],[_1AP,new T(function(){return B(A(_SF,[_1AR,_3r]));})]))){var _1AT=B(_13r(_1AN)),_1AU=_1AT[1],_1AV=new T(function(){return B(_13r(_1AN));}),_1AW=new T(function(){return B(_13t(_1AV));});return new F(function(){return (function(_1AX,_1AY){while(1){var _1AZ=(function(_1B0,_1B1){var _1B2=E(_1AN),_1B3=_1B2[3],_1B4=E(_1B2[1]);if(!B(_1AE(_1B4[1],_1B4[2],_1B4[3],_1B2[4],_1B1))){return !B(A(E(E(_1AT[2])[1])[1],[_1B1,new T(function(){return B(A(_SF,[_1AU,_Je]));})]))?B((function(_1B5,_1B6,_1B7){while(1){var _1B8=(function(_1B9,_1Ba,_1Bb){var _1Bc=E(_1AN),_1Bd=_1Bc[3],_1Be=E(_1Bc[1]);if(!B(_1AE(_1Be[1],_1Be[2],_1Be[3],_1Bc[4],_1Ba))){if(!B(A(new T(function(){return B(_ru(new T(function(){return B(_1AA(new T(function(){return B(_1AC(_1AV));},1)));})));}),[_1Ba,new T(function(){return B(A(_SF,[_1AW,_Je]));})]))){_1B5=new T(function(){return B(A(new T(function(){return B(_1kr(_1AM));}),[_1B9,_1B9]));});_1B6=new T(function(){return B(A(_1Bd,[new T(function(){return B(A(new T(function(){return B(_1kv(_1AW));}),[_1Ba,new T(function(){return B(A(_SF,[_1AW,_Je]));})]));}),new T(function(){return B(A(_SF,[_1AW,_3s]));})]));});_1B7=new T(function(){return B(A(new T(function(){return B(_1kr(_1AM));}),[_1B9,_1Bb]));});return null;}else{return new F(function(){return A(new T(function(){return B(_1kr(_1AM));}),[_1B9,_1Bb]);});}}else{_1B5=new T(function(){return B(A(new T(function(){return B(_1kr(_1AM));}),[_1B9,_1B9]));});_1B6=new T(function(){return B(A(_1Bd,[_1Ba,new T(function(){return B(A(_SF,[_1AW,_3s]));})]));});var _1Bf=_1Bb;_1B7=_1Bf;return null;}})(_1B5,_1B6,_1B7);if(_1B8!=null){return _1B8;}}})(new T(function(){return B(A(new T(function(){return B(_1kr(_1AM));}),[_1B0,_1B0]));}),new T(function(){return B(A(_1B3,[new T(function(){return B(A(new T(function(){return B(_1kv(_1AU));}),[_1B1,new T(function(){return B(A(_SF,[_1AU,_Je]));})]));}),new T(function(){return B(A(_SF,[_1AU,_3s]));})]));}),_1B0)):E(_1B0);}else{_1AX=new T(function(){return B(A(new T(function(){return B(_1kr(_1AM));}),[_1B0,_1B0]));});_1AY=new T(function(){return B(A(_1B3,[_1B1,new T(function(){return B(A(_SF,[_1AU,_3s]));})]));});return null;}})(_1AX,_1AY);if(_1AZ!=null){return _1AZ;}}})(_1AO,_1AP);});}else{return new F(function(){return A(_SF,[_1AM,_Je]);});}}else{return E(_1AK);}},_1Bg=function(_1Bh,_1Bi){var _1Bj=E(_1Bh);return _1Bj[0]==0?_1Bj[1]*Math.pow(2,_1Bi):I_toNumber(_1Bj[1])*Math.pow(2,_1Bi);},_1Bk=function(_1Bl,_1Bm){var _1Bn=B(_1lx(_1Bm)),_1Bo=_1Bn[1],_1Bp=_1Bn[2],_1Bq=new T(function(){return B(_13t(new T(function(){return B(_13r(_1Bl));},1)));});if(_1Bp<0){var _1Br= -_1Bp;if(_1Br>=0){var _1Bs=E(_1Br),_1Bt=_1Bs==0?E(_Je):B(_1Au(_1xr,_1Bs));if(!B(_3j(_1Bt,_IG))){var _1Bu=B(_12z(_1Bo,_1Bt));return [0,new T(function(){return B(A(_SF,[_1Bq,_1Bu[1]]));}),new T(function(){return [0,B(_1Bg(_1Bu[2],_1Bp))];})];}else{return E(_3i);}}else{return E(_Jg);}}else{return [0,new T(function(){return B(A(_1kr,[_1Bq,new T(function(){return B(A(_SF,[_1Bq,_1Bo]));}),new T(function(){return B(_1AL(_1Bq,_1Al,new T(function(){return B(A(_SF,[_1Bq,_1xr]));}),[0,_1Bp]));})]));}),_WL];}},_1Bv=function(_1Bw,_1Bx){var _1By=B(_1Bk(_1Bw,E(_1Bx)[1])),_1Bz=_1By[1];if(E(_1By[2])[1]<=0){return E(_1Bz);}else{var _1BA=E(B(_13r(_1Bw))[1]);return new F(function(){return A(_1BA[1],[_1Bz,new T(function(){return B(A(_1BA[7],[_1lw]));})]);});}},_1BB=function(_1BC,_1BD){var _1BE=B(_1Bk(_1BC,E(_1BD)[1])),_1BF=_1BE[1];if(E(_1BE[2])[1]>=0){return E(_1BF);}else{var _1BG=E(B(_13r(_1BC))[1]);return new F(function(){return A(_1BG[3],[_1BF,new T(function(){return B(A(_1BG[7],[_1lw]));})]);});}},_1BH=function(_1BI,_1BJ){var _1BK=B(_1Bk(_1BI,E(_1BJ)[1]));return [0,_1BK[1],_1BK[2]];},_1BL=function(_1BM,_1BN){var _1BO=B(_1Bk(_1BM,_1BN)),_1BP=_1BO[1],_1BQ=E(_1BO[2])[1],_1BR=new T(function(){var _1BS=E(B(_13r(_1BM))[1]),_1BT=_1BS[7];return _1BQ>=0?B(A(_1BS[1],[_1BP,new T(function(){return B(A(_1BT,[_1lw]));})])):B(A(_1BS[3],[_1BP,new T(function(){return B(A(_1BT,[_1lw]));})]));},1);if(_1BQ<0){var _1BU= -_1BQ-0.5;if(_1BU>=0){if(!E(_1BU)){var _1BV=E(_1BM),_1BW=E(_1BV[1]);return !B(_1AE(_1BW[1],_1BW[2],_1BW[3],_1BV[4],_1BP))?E(_1BR):E(_1BP);}else{return E(_1BR);}}else{return E(_1BP);}}else{var _1BX=_1BQ-0.5;if(_1BX>=0){if(!E(_1BX)){var _1BY=E(_1BM),_1BZ=E(_1BY[1]);return !B(_1AE(_1BZ[1],_1BZ[2],_1BZ[3],_1BY[4],_1BP))?E(_1BR):E(_1BP);}else{return E(_1BR);}}else{return E(_1BP);}}},_1C0=function(_1C1,_1C2){return new F(function(){return _1BL(_1C1,E(_1C2)[1]);});},_1C3=function(_1C4,_1C5){return E(B(_1Bk(_1C4,E(_1C5)[1]))[1]);},_1C6=[0,_1yb,_X6,_1BH,_1C3,_1C0,_1Bv,_1BB],_1C7=function(_1C8,_1C9){return E(_1C8)[1]!=E(_1C9)[1]?true:false;},_1Ca=function(_1Cb,_1Cc){return E(_1Cb)[1]==E(_1Cc)[1];},_1Cd=[0,_1Ca,_1C7],_1Ce=function(_1Cf,_1Cg){return E(_1Cf)[1]<E(_1Cg)[1];},_1Ch=function(_1Ci,_1Cj){return E(_1Ci)[1]<=E(_1Cj)[1];},_1Ck=function(_1Cl,_1Cm){return E(_1Cl)[1]>E(_1Cm)[1];},_1Cn=function(_1Co,_1Cp){return E(_1Co)[1]>=E(_1Cp)[1];},_1Cq=function(_1Cr,_1Cs){var _1Ct=E(_1Cr)[1],_1Cu=E(_1Cs)[1];return _1Ct>=_1Cu?_1Ct!=_1Cu?2:1:0;},_1Cv=function(_1Cw,_1Cx){var _1Cy=E(_1Cw),_1Cz=E(_1Cx);return _1Cy[1]>_1Cz[1]?E(_1Cy):E(_1Cz);},_1CA=function(_1CB,_1CC){var _1CD=E(_1CB),_1CE=E(_1CC);return _1CD[1]>_1CE[1]?E(_1CE):E(_1CD);},_1CF=[0,_1Cd,_1Cq,_1Ce,_1Cn,_1Ck,_1Ch,_1Cv,_1CA],_1CG=function(_1CH,_1CI){while(1){var _1CJ=(function(_1CK,_1CL){var _1CM=E(_1lA)[1]["v"]["i8"][(255&_1CK>>>0)>>>0&4294967295];if(_1CL>_1CM){if(_1CM>=8){var _1CN=_1CK>>8,_1CO=_1CL-8|0;_1CH=_1CN;_1CI=_1CO;return null;}else{return [0,new T(function(){return B(_qN(_1CK>>_1CM));}),_1CL-_1CM|0];}}else{return [0,new T(function(){return B(_qN(_1CK>>_1CL));}),0];}})(_1CH,_1CI);if(_1CJ!=null){return _1CJ;}}},_1CP=function(_1CQ){var _1CR=decodeFloat(_1CQ),_1CS=_1CR[1],_1CT=_1CR[2];if(_1CT<0){var _1CU=function(_1CV){if(!_1CV){return [0,B(_qN(_1CS)),B(_1m6(_1lw, -_1CT))];}else{var _1CW=B(_1CG(_1CS, -_1CT));return [0,E(_1CW[1]),B(_1m6(_1lw,_1CW[2]))];}};return (_1CS>>>0&1)>>>0==0?B(_1CU(1)):B(_1CU(0));}else{return [0,B(_1m6(B(_qN(_1CS)),_1CT)),_1lw];}},_1CX=function(_1CY){var _1CZ=B(_1CP(E(_1CY)[1]));return [0,E(_1CZ[1]),E(_1CZ[2])];},_1D0=[0,_1wO,_1CF,_1CX],_1D1=[0,-1],_1D2=[0,1],_1D3=function(_1D4,_1D5){var _1D6=E(_1D4);return _1D6[0]==0?_1D6[1]*Math.pow(2,_1D5):I_toNumber(_1D6[1])*Math.pow(2,_1D5);},_1D7=function(_1D8,_1D9){var _1Da=decodeFloat(_1D9),_1Db=_1Da[1],_1Dc=_1Da[2],_1Dd=new T(function(){return B(_13t(new T(function(){return B(_13r(_1D8));},1)));});if(_1Dc<0){var _1De=new T(function(){if(_1Db<0){var _1Df= -_1Dc;if(_1Df<32){var _1Dg=[0, -( -_1Db>>_1Df)];}else{var _1Dg= -_1Db>=0?E(_Li):E(_1D2);}var _1Dh=_1Dg,_1Di=_1Dh,_1Dj=_1Di;}else{var _1Dk= -_1Dc;if(_1Dk<32){var _1Dl=[0,_1Db>>_1Dk];}else{var _1Dl=_1Db>=0?E(_Li):E(_1D1);}var _1Dm=_1Dl,_1Dn=_1Dm,_1Dj=_1Dn;}var _1Do=_1Dj;return _1Do;});return [0,new T(function(){return B(A(_SF,[_1Dd,new T(function(){return B(_qN(E(_1De)[1]));})]));}),new T(function(){var _1Dp= -_1Dc;if(_1Dp<32){var _1Dq=[0,B(_1D3(B(_qN(_1Db-(E(_1De)[1]<<_1Dp)|0)),_1Dc))];}else{var _1Dq=[0,B(_1D3(B(_qN(_1Db)),_1Dc))];}var _1Dr=_1Dq,_1Ds=_1Dr,_1Dt=_1Ds;return _1Dt;})];}else{return [0,new T(function(){return B(A(_1kr,[_1Dd,new T(function(){return B(A(_SF,[_1Dd,new T(function(){return B(_qN(_1Db));})]));}),new T(function(){return B(_1AL(_1Dd,_1Al,new T(function(){return B(A(_SF,[_1Dd,_1xr]));}),[0,_1Dc]));})]));}),_1wx];}},_1Du=function(_1Dv,_1Dw){var _1Dx=B(_1D7(_1Dv,E(_1Dw)[1])),_1Dy=_1Dx[1];if(E(_1Dx[2])[1]<=0){return E(_1Dy);}else{var _1Dz=E(B(_13r(_1Dv))[1]);return new F(function(){return A(_1Dz[1],[_1Dy,new T(function(){return B(A(_1Dz[7],[_1lw]));})]);});}},_1DA=function(_1DB,_1DC){var _1DD=B(_1D7(_1DB,E(_1DC)[1])),_1DE=_1DD[1];if(E(_1DD[2])[1]>=0){return E(_1DE);}else{var _1DF=E(B(_13r(_1DB))[1]);return new F(function(){return A(_1DF[3],[_1DE,new T(function(){return B(A(_1DF[7],[_1lw]));})]);});}},_1DG=function(_1DH,_1DI){var _1DJ=B(_1D7(_1DH,E(_1DI)[1]));return [0,_1DJ[1],_1DJ[2]];},_1DK=function(_1DL,_1DM){var _1DN=B(_1D7(_1DL,_1DM)),_1DO=_1DN[1],_1DP=E(_1DN[2])[1],_1DQ=new T(function(){var _1DR=E(B(_13r(_1DL))[1]),_1DS=_1DR[7];return _1DP>=0?B(A(_1DR[1],[_1DO,new T(function(){return B(A(_1DS,[_1lw]));})])):B(A(_1DR[3],[_1DO,new T(function(){return B(A(_1DS,[_1lw]));})]));},1);if(_1DP<0){var _1DT= -_1DP-0.5;if(_1DT>=0){if(!E(_1DT)){var _1DU=E(_1DL),_1DV=E(_1DU[1]);return !B(_1AE(_1DV[1],_1DV[2],_1DV[3],_1DU[4],_1DO))?E(_1DQ):E(_1DO);}else{return E(_1DQ);}}else{return E(_1DO);}}else{var _1DW=_1DP-0.5;if(_1DW>=0){if(!E(_1DW)){var _1DX=E(_1DL),_1DY=E(_1DX[1]);return !B(_1AE(_1DY[1],_1DY[2],_1DY[3],_1DX[4],_1DO))?E(_1DQ):E(_1DO);}else{return E(_1DQ);}}else{return E(_1DO);}}},_1DZ=function(_1E0,_1E1){return new F(function(){return _1DK(_1E0,E(_1E1)[1]);});},_1E2=function(_1E3,_1E4){return E(B(_1D7(_1E3,E(_1E4)[1]))[1]);},_1E5=[0,_1D0,_1wS,_1DG,_1E2,_1DZ,_1Du,_1DA],_1E6=function(_1E7,_1E8,_1E9){while(1){if(!(_1E8%2)){var _1Ea=_1E7*_1E7,_1Eb=quot(_1E8,2);_1E7=_1Ea;_1E8=_1Eb;continue;}else{var _1Ec=E(_1E8);if(_1Ec==1){return _1E7*_1E9;}else{var _1Ea=_1E7*_1E7;_1E8=quot(_1Ec-1|0,2);var _1Ed=_1E7*_1E9;_1E7=_1Ea;_1E9=_1Ed;continue;}}}},_1Ee=function(_1Ef,_1Eg){while(1){if(!(_1Eg%2)){var _1Eh=_1Ef*_1Ef,_1Ei=quot(_1Eg,2);_1Ef=_1Eh;_1Eg=_1Ei;continue;}else{var _1Ej=E(_1Eg);if(_1Ej==1){return E(_1Ef);}else{return new F(function(){return _1E6(_1Ef*_1Ef,quot(_1Ej-1|0,2),_1Ef);});}}}},_1Ek=function(_1El){return new F(function(){return err(B(unAppCStr("Char.intToDigit: not a digit ",new T(function(){if(_1El>=0){var _1Em=jsShowI(_1El),_1En=_1Em,_1Eo=fromJSStr(_1En);}else{var _1Ep=jsShowI(_1El),_1Eq=_1Ep,_1Eo=fromJSStr(_1Eq);}var _1Er=_1Eo;return _1Er;}))));});},_1Es=function(_1Et){var _1Eu=function(_1Ev){if(_1Et<10){return new F(function(){return _1Ek(_1Et);});}else{if(_1Et>15){return new F(function(){return _1Ek(_1Et);});}else{return (97+_1Et|0)-10|0;}}};if(_1Et<0){return new F(function(){return _1Eu(_);});}else{if(_1Et>9){return new F(function(){return _1Eu(_);});}else{return 48+_1Et|0;}}},_1Ew=function(_1Ex){return [0,B(_1Es(E(_1Ex)[1]))];},_1Ey=new T(function(){return B(_QM("GHC/Float.lhs:619:11-64|d : ds\'"));}),_1Ez=function(_1EA,_1EB){if(E(_1EA)[1]<=0){var _1EC=B(_8A(_1Ew,[1,_Li,_1EB]));return _1EC[0]==0?E(_1Ey):[0,_1EC[1],_1EC[2]];}else{var _1ED=B(_8A(_1Ew,_1EB));return _1ED[0]==0?E(_1Ey):[0,_1ED[1],_1ED[2]];}},_1EE=function(_1EF){return E(E(_1EF)[1]);},_1EG=function(_1EH){return E(E(_1EH)[1]);},_1EI=function(_1EJ){return E(E(_1EJ)[1]);},_1EK=[0,48],_1EL=[1,_1EK,_1g],_1EM=[0,46],_1EN=function(_1EO,_1EP,_1EQ){while(1){var _1ER=(function(_1ES,_1ET,_1EU){var _1EV=E(_1ES);if(!_1EV){var _1EW=B(_1vg(_1ET,_1g));return _1EW[0]==0?[1,_1EK,[1,_1EM,new T(function(){var _1EX=E(_1EU);return _1EX[0]==0?E(_1EL):E(_1EX);})]]:B(_2J(_1EW,[1,_1EM,new T(function(){var _1EY=E(_1EU);return _1EY[0]==0?E(_1EL):E(_1EY);})]));}else{var _1EZ=E(_1EU);if(!_1EZ[0]){_1EO=_1EV-1|0;var _1F0=[1,_1EK,_1ET];_1EQ=_1g;_1EP=_1F0;return null;}else{_1EO=_1EV-1|0;var _1F0=[1,_1EZ[1],_1ET];_1EQ=_1EZ[2];_1EP=_1F0;return null;}}})(_1EO,_1EP,_1EQ);if(_1ER!=null){return _1ER;}}},_1F1=[0,0],_1F2=function(_1F3,_1F4,_1F5){return new F(function(){return A(_1F3,[[1,_2W,new T(function(){return B(A(_1F4,[_1F5]));})]]);});},_1F6=new T(function(){return B(unCStr("foldr1"));}),_1F7=new T(function(){return B(_1lj(_1F6));}),_1F8=function(_1F9,_1Fa){var _1Fb=E(_1Fa);if(!_1Fb[0]){return E(_1F7);}else{var _1Fc=_1Fb[1],_1Fd=E(_1Fb[2]);if(!_1Fd[0]){return E(_1Fc);}else{return new F(function(){return A(_1F9,[_1Fc,new T(function(){return B(_1F8(_1F9,_1Fd));})]);});}}},_1Fe=new T(function(){return B(unCStr(" out of range "));}),_1Ff=new T(function(){return B(unCStr("}.index: Index "));}),_1Fg=new T(function(){return B(unCStr("Ix{"));}),_1Fh=[1,_5n,_1g],_1Fi=[1,_5n,_1Fh],_1Fj=function(_1Fk,_1Fl,_1Fm,_1Fn,_1Fo){return new F(function(){return err(B(_2J(_1Fg,new T(function(){return B(_2J(_1Fk,new T(function(){return B(_2J(_1Ff,[1,_5o,new T(function(){return B(A(_1Fo,[_1F1,_1Fl,[1,_5n,new T(function(){return B(_2J(_1Fe,[1,_5o,[1,_5o,new T(function(){return B(A(_1F8,[_1F2,[1,new T(function(){return B(A(_1Fo,[_B8,_1Fm]));}),[1,new T(function(){return B(A(_1Fo,[_B8,_1Fn]));}),_1g]],_1Fi]));})]]));})]]));})]));},1)));},1))));});},_1Fp=function(_1Fq,_1Fr,_1Fs,_1Ft){var _1Fu=E(_1Fs);return new F(function(){return _1Fj(_1Fq,_1Fr,_1Fu[1],_1Fu[2],E(_1Ft)[1]);});},_1Fv=function(_1Fw,_1Fx,_1Fy,_1Fz){return new F(function(){return _1Fp(_1Fz,_1Fy,_1Fx,_1Fw);});},_1FA=new T(function(){return B(unCStr("Int"));}),_1FB=function(_1FC,_1FD,_1FE){return new F(function(){return _1Fv(_zl,[0,_1FD,_1FE],_1FC,_1FA);});},_1FF=new T(function(){return B(unCStr("(Array.!): undefined array element"));}),_1FG=new T(function(){return B(err(_1FF));}),_1FH=[0,1100],_1FI=[0,_Li,_1FH],_1FJ=function(_1FK){return new F(function(){return _1Fv(_zl,_1FI,[0,_1FK],_1FA);});},_1FL=function(_){var _1FM=newArr(1101,_1FG),_1FN=_1FM;return new F(function(){return (function(_1FO,_){while(1){var _1FP=(function(_1FQ,_){if(0>_1FQ){return new F(function(){return _1FJ(_1FQ);});}else{if(_1FQ>1100){return new F(function(){return _1FJ(_1FQ);});}else{var _=_1FN[_1FQ]=new T(function(){if(_1FQ>=0){var _1FR=E(_1FQ),_1FS=_1FR==0?E(_Je):B(_1Au(_1xr,_1FR));}else{var _1FS=E(_Jg);}var _1FT=_1FS;return _1FT;}),_1FU=E(_1FQ);if(_1FU==1100){var _1FV=_1FN,_1FW=_1FV;return [0,E(_Li),E(_1FH),1101,_1FW];}else{_1FO=_1FU+1|0;return null;}}}})(_1FO,_);if(_1FP!=null){return _1FP;}}})(0,_);});},_1FX=function(_1FY){var _1FZ=B(A(_1FY,[_])),_1G0=_1FZ;return E(_1G0);},_1G1=new T(function(){return B(_1FX(_1FL));}),_1G2=[0,10],_1G3=[0,324],_1G4=[0,_Li,_1G3],_1G5=function(_1G6){return new F(function(){return _1Fv(_zl,_1G4,[0,_1G6],_1FA);});},_1G7=function(_){var _1G8=newArr(325,_1FG),_1G9=_1G8;return new F(function(){return (function(_1Ga,_){while(1){var _1Gb=(function(_1Gc,_){if(0>_1Gc){return new F(function(){return _1G5(_1Gc);});}else{if(_1Gc>324){return new F(function(){return _1G5(_1Gc);});}else{var _=_1G9[_1Gc]=new T(function(){if(_1Gc>=0){var _1Gd=E(_1Gc),_1Ge=_1Gd==0?E(_Je):B(_1Au(_1G2,_1Gd));}else{var _1Ge=E(_Jg);}var _1Gf=_1Ge;return _1Gf;}),_1Gg=E(_1Gc);if(_1Gg==324){var _1Gh=_1G9,_1Gi=_1Gh;return [0,E(_Li),E(_1G3),325,_1Gi];}else{_1Ga=_1Gg+1|0;return null;}}}})(_1Ga,_);if(_1Gb!=null){return _1Gb;}}})(0,_);});},_1Gj=new T(function(){return B(_1FX(_1G7));}),_1Gk=function(_1Gl,_1Gm){var _1Gn=[0,_1Gm],_1Go=function(_1Gp){if(!B(_3j(_1Gl,_1G2))){if(_1Gm>=0){var _1Gq=E(_1Gm);return _1Gq==0?E(_Je):B(_1Au(_1Gl,_1Gq));}else{return E(_Jg);}}else{if(_1Gm>324){if(_1Gm>=0){var _1Gr=E(_1Gm);return _1Gr==0?E(_Je):B(_1Au(_1Gl,_1Gr));}else{return E(_Jg);}}else{var _1Gs=E(_1Gj),_1Gt=E(_1Gs[1]),_1Gu=_1Gt[1],_1Gv=E(_1Gs[2]);if(_1Gu>_1Gm){return new F(function(){return _1FB(_1Gn,_1Gt,_1Gv);});}else{if(_1Gm>_1Gv[1]){return new F(function(){return _1FB(_1Gn,_1Gt,_1Gv);});}else{return E(_1Gs[4][_1Gm-_1Gu|0]);}}}}};if(!B(_3j(_1Gl,_1xr))){return new F(function(){return _1Go(_);});}else{if(_1Gm<0){return new F(function(){return _1Go(_);});}else{if(_1Gm>1100){return new F(function(){return _1Go(_);});}else{var _1Gw=E(_1G1),_1Gx=E(_1Gw[1]),_1Gy=_1Gx[1],_1Gz=E(_1Gw[2]);if(_1Gy>_1Gm){return new F(function(){return _1FB(_1Gn,_1Gx,_1Gz);});}else{if(_1Gm>_1Gz[1]){return new F(function(){return _1FB(_1Gn,_1Gx,_1Gz);});}else{return E(_1Gw[4][_1Gm-_1Gy|0]);}}}}}},_1GA=[1,_Li,_1g],_1GB=function(_1GC,_1GD,_1GE,_1GF,_1GG,_1GH,_1GI,_1GJ){if(!B(A(_1GC,[_1GJ,new T(function(){return B(A(_SF,[B(_1EG(B(_1EE(_1GD)))),_IG]));})]))){var _1GK=new T(function(){return B(A(_1GE,[_1GJ]));}),_1GL=new T(function(){return B(A(_1GF,[_1GJ]));}),_1GM=new T(function(){return [0,E(B(A(_1GG,[_1GJ]))[1])[1]-E(_1GL)[1]|0];}),_1GN=new T(function(){return B(A(_1GH,[_1GJ]));}),_1GO=new T(function(){return E(E(_1GN)[2]);}),_1GP=new T(function(){var _1GQ=E(_1GO),_1GR=_1GQ[1],_1GS=E(_1GM)[1]-_1GR|0;if(_1GS<=0){var _1GT=[0,new T(function(){return E(E(_1GN)[1]);}),_1GQ];}else{var _1GT=[0,new T(function(){var _1GU=B(_1Gk(_1GK,_1GS));if(!B(_3j(_1GU,_IG))){var _1GV=B(_3H(E(_1GN)[1],_1GU));}else{var _1GV=E(_3i);}var _1GW=_1GV;return _1GW;}),[0,_1GR+_1GS|0]];}var _1GX=_1GT,_1GY=_1GX,_1GZ=_1GY,_1H0=_1GZ;return _1H0;}),_1H1=new T(function(){return E(E(_1GP)[2]);}),_1H2=new T(function(){return E(E(_1GP)[1]);}),_1H3=new T(function(){var _1H4=E(_1H1)[1];if(_1H4<0){if(_1H4<=E(_1GM)[1]){var _1H5=[0,new T(function(){return B(_qP(_1H2,_1xr));}),new T(function(){return B(_qP(B(_1Gk(_1GK, -_1H4)),_1xr));}),_1lw,_1lw];}else{var _1H5=!B(_3j(_1H2,B(_1Gk(_1GK,E(_1GL)[1]-1|0))))?[0,new T(function(){return B(_qP(_1H2,_1xr));}),new T(function(){return B(_qP(B(_1Gk(_1GK, -_1H4)),_1xr));}),_1lw,_1lw]:[0,new T(function(){return B(_qP(B(_qP(_1H2,_1GK)),_1xr));}),new T(function(){return B(_qP(B(_1Gk(_1GK, -_1H4+1|0)),_1xr));}),_1GK,_1lw];}var _1H6=_1H5,_1H7=_1H6,_1H8=_1H7;}else{var _1H9=new T(function(){return B(_1Gk(_1GK,_1H4));}),_1H8=!B(_3j(_1H2,B(_1Gk(_1GK,E(_1GL)[1]-1|0))))?[0,new T(function(){return B(_qP(B(_qP(_1H2,_1H9)),_1xr));}),_1xr,_1H9,_1H9]:[0,new T(function(){return B(_qP(B(_qP(B(_qP(_1H2,_1H9)),_1GK)),_1xr));}),new T(function(){return B(_qP(_1xr,_1GK));}),new T(function(){return B(_qP(_1H9,_1GK));}),_1H9];}var _1Ha=_1H8,_1Hb=_1Ha;return _1Hb;}),_1Hc=new T(function(){return E(E(_1H3)[2]);}),_1Hd=new T(function(){return E(E(_1H3)[3]);}),_1He=new T(function(){return E(E(_1H3)[1]);}),_1Hf=new T(function(){var _1Hg=new T(function(){return B(_qx(_1He,_1Hd));}),_1Hh=function(_1Hi){var _1Hj=(Math.log(B(_1ws(B(_qx(_1H2,_1lw)))))+E(_1H1)[1]*Math.log(B(_1ws(_1GK))))/Math.log(B(_1ws(_1GI))),_1Hk=_1Hj&4294967295;return _1Hk>=_1Hj?E(_1Hk):_1Hk+1|0;},_1Hl=function(_1Hm){while(1){if(_1Hm<0){if(!B(_s5(B(_qP(B(_1Gk(_1GI, -_1Hm)),_1Hg)),_1Hc))){var _1Hn=_1Hm+1|0;_1Hm=_1Hn;continue;}else{return E(_1Hm);}}else{if(!B(_s5(_1Hg,B(_qP(B(_1Gk(_1GI,_1Hm)),_1Hc))))){var _1Hn=_1Hm+1|0;_1Hm=_1Hn;continue;}else{return E(_1Hm);}}}};if(!B(_3j(_1GK,_1xr))){var _1Ho=[0,B(_1Hl(B(_1Hh(_))))];}else{if(!B(_3j(_1GI,_1G2))){var _1Hp=[0,B(_1Hl(B(_1Hh(_))))];}else{var _1Hq=(E(_1GL)[1]-1|0)+E(_1GO)[1]|0;if(_1Hq<0){var _1Hr=[0,B(_1Hl(quot(imul(_1Hq,8651)|0,28738)))];}else{var _1Hr=[0,B(_1Hl(quot(imul(_1Hq,8651)|0,28738)+1|0))];}var _1Hs=_1Hr,_1Ht=_1Hs,_1Hu=_1Ht,_1Hv=_1Hu,_1Hw=_1Hv,_1Hp=_1Hw;}var _1Ho=_1Hp;}return _1Ho;});return [0,new T(function(){var _1Hx=E(_1Hf)[1],_1Hy=function(_1Hz,_1HA,_1HB,_1HC,_1HD){while(1){var _1HE=(function(_1HF,_1HG,_1HH,_1HI,_1HJ){if(!B(_3j(_1HH,_IG))){var _1HK=B(_12z(B(_qP(_1HG,_1GI)),_1HH)),_1HL=_1HK[1],_1HM=_1HK[2],_1HN=B(_qP(_1HJ,_1GI)),_1HO=B(_qP(_1HI,_1GI));if(!B(_4d(_1HM,_1HN))){if(!B(_K9(B(_qx(_1HM,_1HO)),_1HH))){var _1HP=[1,_1HL,_1HF];_1HA=_1HM;var _1HQ=_1HH;_1HC=_1HO;_1HD=_1HN;_1Hz=_1HP;_1HB=_1HQ;return null;}else{return [1,new T(function(){return B(_qx(_1HL,_1lw));}),_1HF];}}else{return !B(_K9(B(_qx(_1HM,_1HO)),_1HH))?[1,_1HL,_1HF]:!B(_4d(B(_qP(_1HM,_1xr)),_1HH))?[1,new T(function(){return B(_qx(_1HL,_1lw));}),_1HF]:[1,_1HL,_1HF];}}else{return E(_3i);}})(_1Hz,_1HA,_1HB,_1HC,_1HD);if(_1HE!=null){return _1HE;}}};if(_1Hx<0){var _1HR=B(_1Gk(_1GI, -_1Hx)),_1HS=B(_8A(_Rm,B(_1vg(B(_1Hy(_1g,B(_qP(_1He,_1HR)),_1Hc,B(_qP(_1Hd,_1HR)),B(_qP(E(_1H3)[4],_1HR)))),_1g))));}else{var _1HS=B(_8A(_Rm,B(_1vg(B(_1Hy(_1g,_1He,B(_qP(_1Hc,B(_1Gk(_1GI,_1Hx)))),_1Hd,E(_1H3)[4])),_1g))));}var _1HT=_1HS,_1HU=_1HT;return _1HU;}),_1Hf];}else{return [0,_1GA,_Li];}},_1HV=function(_1HW,_1HX){while(1){var _1HY=E(_1HX);if(!_1HY[0]){return true;}else{if(!B(A(_1HW,[_1HY[1]]))){return false;}else{_1HX=_1HY[2];continue;}}}},_1HZ=function(_1I0){return E(_1I0)[1]%2==0?true:false;},_1I1=new T(function(){return B(unCStr("roundTo: bad Value"));}),_1I2=new T(function(){return B(err(_1I1));}),_1I3=function(_1I4){return E(E(_1I4)[1])==0?true:false;},_1I5=function(_1I6){return _1I6>1?[1,_Li,new T(function(){return B(_1I5(_1I6-1|0));})]:E(_1GA);},_1I7=function(_1I8,_1I9,_1Ia){var _1Ib=function(_1Ic,_1Id,_1Ie){var _1If=E(_1Ie);if(!_1If[0]){return [0,_Li,new T(function(){var _1Ig=E(_1Ic)[1];return _1Ig>0?B(_1I5(_1Ig)):[0];})];}else{var _1Ih=_1If[1],_1Ii=_1If[2],_1Ij=E(E(_1Ic)[1]);if(!_1Ij){var _1Ik=E(_1Ih)[1],_1Il=E(new T(function(){return [0,quot(E(_1I8)[1],2)];}))[1];return _1Ik!=_1Il?[0,new T(function(){return _1Ik<_1Il?E(_Li):E(_1D2);}),_1g]:!E(_1Id)?[0,new T(function(){return _1Ik<_1Il?E(_Li):E(_1D2);}),_1g]:!B(_1HV(_1I3,_1Ii))?[0,new T(function(){return _1Ik<_1Il?E(_Li):E(_1D2);}),_1g]:[0,_Li,_1g];}else{var _1Im=B(_1Ib([0,_1Ij-1|0],new T(function(){return B(_1HZ(_1Ih));},1),_1Ii)),_1In=_1Im[2],_1Io=E(_1Im[1])[1]+E(_1Ih)[1]|0;return _1Io!=E(_1I8)[1]?[0,_Li,[1,[0,_1Io],_1In]]:[0,_1D2,[1,_Li,_1In]];}}},_1Ip=B(_1Ib(_1I9,_gw,_1Ia));switch(E(E(_1Ip[1])[1])){case 0:return E(_1Ip);case 1:return [0,_1D2,[1,_1D2,_1Ip[2]]];default:return E(_1I2);}},_1Iq=function(_1Ir){return E(E(_1Ir)[3]);},_1Is=0,_1It=1,_1Iu=[0,10],_1Iv=new T(function(){return B(unCStr("e0"));}),_1Iw=function(_1Ix,_1Iy){var _1Iz=E(_1Ix);if(!_1Iz[0]){return E(_1Iv);}else{var _1IA=_1Iz[1];return _1Iy>1?[1,_1IA,new T(function(){return B(_1Iw(_1Iz[2],_1Iy-1|0));})]:[1,_1IA,_1Iv];}},_1IB=new T(function(){return B(_QM("GHC/Float.lhs:591:12-70|(d : ds\')"));}),_1IC=[0,101],_1ID=new T(function(){return B(unCStr("Infinity"));}),_1IE=new T(function(){return B(unCStr("-Infinity"));}),_1IF=new T(function(){return B(unCStr("NaN"));}),_1IG=new T(function(){return B(unCStr("formatRealFloat/doFmt/FFExponent: []"));}),_1IH=new T(function(){return B(err(_1IG));}),_1II=new T(function(){return B(unCStr("0.0e0"));}),_1IJ=function(_1IK){return E(E(_1IK)[4]);},_1IL=new T(function(){return [1,_1EK,_1IL];}),_1IM=function(_1IN,_1IO,_1IP,_1IQ,_1IR,_1IS,_1IT,_1IU,_1IV,_1IW,_1IX,_1IY){if(!B(A(_1IT,[_1IY]))){var _1IZ=new T(function(){return B(_1EG(new T(function(){return B(_1EE(_1IO));},1)));});if(!B(A(_1IU,[_1IY]))){var _1J0=function(_1J1,_1J2,_1J3){while(1){var _1J4=(function(_1J5,_1J6,_1J7){switch(E(_1J5)){case 0:var _1J8=E(_1IX);if(!_1J8[0]){var _1J9=B(_8A(_1Ew,_1J6));if(!_1J9[0]){return E(_1IH);}else{var _1Ja=_1J9[2],_1Jb=E(_1J9[1]),_1Jc=function(_1Jd){var _1Je=E(_1Ja);return _1Je[0]==0?[1,_1Jb,new T(function(){return B(unAppCStr(".0e",new T(function(){return B(_5p(0,E(_1J7)[1]-1|0,_1g));})));})]:[1,_1Jb,[1,_1EM,new T(function(){return B(_2J(_1Je,[1,_1IC,new T(function(){return B(_5p(0,E(_1J7)[1]-1|0,_1g));})]));})]];};return E(_1Jb[1])==48?E(_1Ja)[0]==0?E(_1II):B(_1Jc(_)):B(_1Jc(_));}}else{var _1Jf=new T(function(){var _1Jg=E(_1J8[1]);return _1Jg[1]>1?E(_1Jg):E(_1D2);},1),_1Jh=function(_1Ji){var _1Jj=new T(function(){var _1Jk=B(_1I7(_1Iu,new T(function(){return [0,E(_1Jf)[1]+1|0];},1),_1J6));return [0,_1Jk[1],_1Jk[2]];}),_1Jl=new T(function(){return E(E(_1Jj)[1]);}),_1Jm=new T(function(){if(E(_1Jl)[1]<=0){var _1Jn=B(_8A(_1Ew,E(_1Jj)[2])),_1Jo=_1Jn[0]==0?E(_1IB):[0,_1Jn[1],_1Jn[2]];}else{var _1Jp=E(E(_1Jj)[2]);if(!_1Jp[0]){var _1Jq=E(_1oC);}else{var _1Jr=B(_8A(_1Ew,B(_1ox(_1Jp[1],_1Jp[2])))),_1Jq=_1Jr[0]==0?E(_1IB):[0,_1Jr[1],_1Jr[2]];}var _1Js=_1Jq,_1Jo=_1Js;}var _1Jt=_1Jo,_1Ju=_1Jt;return _1Ju;});return [1,new T(function(){return E(E(_1Jm)[1]);}),[1,_1EM,new T(function(){return B(_2J(E(_1Jm)[2],[1,_1IC,new T(function(){return B(_5p(0,(E(_1J7)[1]-1|0)+E(_1Jl)[1]|0,_1g));})]));})]];},_1Jv=E(_1J6);if(!_1Jv[0]){return new F(function(){return _1Jh(_);});}else{return E(E(_1Jv[1])[1])==0?E(_1Jv[2])[0]==0?[1,_1EK,[1,_1EM,new T(function(){var _1Jw=E(_1Jf)[1];return _1Jw>0?B(_1Iw(_1IL,_1Jw)):E(_1Iv);})]]:B(_1Jh(_)):B(_1Jh(_));}}break;case 1:var _1Jx=E(_1IX);if(!_1Jx[0]){var _1Jy=E(_1J7)[1];return _1Jy>0?B(_1EN(_1Jy,_1g,new T(function(){return B(_8A(_1Ew,_1J6));},1))):B(unAppCStr("0.",new T(function(){var _1Jz= -_1Jy;if(_1Jz>0){var _1JA=function(_1JB){return _1JB>1?[1,_1EK,new T(function(){return B(_1JA(_1JB-1|0));})]:E([1,_1EK,new T(function(){return B(_8A(_1Ew,_1J6));})]);},_1JC=B(_1JA(_1Jz));}else{var _1JC=B(_8A(_1Ew,_1J6));}var _1JD=_1JC,_1JE=_1JD;return _1JE;})));}else{var _1JF=_1Jx[1],_1JG=E(_1J7),_1JH=_1JG[1];if(_1JH<0){var _1JI=new T(function(){var _1JJ= -_1JH;if(_1JJ>0){var _1JK=function(_1JL){return _1JL>1?[1,_Li,new T(function(){return B(_1JK(_1JL-1|0));})]:E([1,_Li,_1J6]);},_1JM=B(_1I7(_1Iu,new T(function(){var _1JN=E(_1JF);return _1JN[1]>0?E(_1JN):E(_Li);},1),B(_1JK(_1JJ)))),_1JO=B(_1Ez(_1JM[1],_1JM[2]));}else{var _1JP=B(_1I7(_1Iu,new T(function(){var _1JQ=E(_1JF);return _1JQ[1]>0?E(_1JQ):E(_Li);},1),_1J6)),_1JO=B(_1Ez(_1JP[1],_1JP[2]));}var _1JR=_1JO,_1JS=_1JR;return _1JS;});return [1,new T(function(){return E(E(_1JI)[1]);}),new T(function(){var _1JT=E(E(_1JI)[2]);return _1JT[0]==0?[0]:[1,_1EM,_1JT];})];}else{var _1JU=B(_1I7(_1Iu,new T(function(){var _1JV=E(_1JF)[1];if(_1JV>0){var _1JW=[0,_1JV+_1JH|0];}else{var _1JW=E(_1JG);}var _1JX=_1JW,_1JY=_1JX;return _1JY;},1),_1J6)),_1JZ=_1JU[2],_1K0=_1JH+E(_1JU[1])[1]|0;if(_1K0>=0){var _1K1=B(_jV(_1K0,new T(function(){return B(_8A(_1Ew,_1JZ));}))),_1K2=_1K1[2],_1K3=E(_1K1[1]);return _1K3[0]==0?[1,_1EK,new T(function(){var _1K4=E(_1K2);return _1K4[0]==0?[0]:[1,_1EM,_1K4];})]:B(_2J(_1K3,new T(function(){var _1K5=E(_1K2);return _1K5[0]==0?[0]:[1,_1EM,_1K5];},1)));}else{return [1,_1EK,new T(function(){var _1K6=B(_8A(_1Ew,_1JZ));return _1K6[0]==0?[0]:[1,_1EM,_1K6];})];}}}break;default:var _1K7=E(_1J7),_1K8=_1K7[1];if(_1K8>=0){if(_1K8<=7){_1J1=_1It;var _1K9=_1J6;_1J3=_1K7;_1J2=_1K9;return null;}else{_1J1=_1Is;var _1K9=_1J6;_1J3=_1K7;_1J2=_1K9;return null;}}else{_1J1=_1Is;var _1K9=_1J6;_1J3=_1K7;_1J2=_1K9;return null;}}})(_1J1,_1J2,_1J3);if(_1J4!=null){return _1J4;}}},_1Ka=function(_1Kb){return [1,_L4,new T(function(){var _1Kc=B(_1GB(E(E(E(E(_1IN)[1])[2])[1])[1],_1IO,_1IP,_1IQ,_1IR,_1IS,_1G2,new T(function(){return B(A(_1IJ,[_1IZ,_1IY]));})));return B(_1J0(_1IW,_1Kc[1],_1Kc[2]));})];};if(!B(A(_1Iq,[B(_1AC(B(_1EI(_1IN)))),_1IY,new T(function(){return B(A(_SF,[_1IZ,_IG]));})]))){if(!B(A(_1IV,[_1IY]))){var _1Kd=B(_1GB(E(E(E(E(_1IN)[1])[2])[1])[1],_1IO,_1IP,_1IQ,_1IR,_1IS,_1G2,_1IY));return new F(function(){return _1J0(_1IW,_1Kd[1],_1Kd[2]);});}else{return new F(function(){return _1Ka(_);});}}else{return new F(function(){return _1Ka(_);});}}else{return !B(A(_1Iq,[B(_1AC(B(_1EI(_1IN)))),_1IY,new T(function(){return B(A(_SF,[_1IZ,_IG]));})]))?E(_1ID):E(_1IE);}}else{return E(_1IF);}},_1Ke=function(_1Kf){var _1Kg=u_towlower(_1Kf),_1Kh=_1Kg;return _1Kh>>>0>1114111?B(_s0(_1Kh)):_1Kh;},_1Ki=function(_1Kj){return new F(function(){return err(B(unAppCStr("Printf.printf: ",_1Kj)));});},_1Kk=new T(function(){return B(unCStr("bad argument"));}),_1Kl=new T(function(){return B(_1Ki(_1Kk));}),_1Km=new T(function(){return B(unCStr("Printf.dfmt: impossible"));}),_1Kn=new T(function(){return B(err(_1Km));}),_1Ko=[0,45],_1Kp=[1,_1Ko,_1g],_1Kq=new T(function(){return B(err(_1Km));}),_1Kr=new T(function(){return B(unCStr("Negative exponent"));}),_1Ks=new T(function(){return B(err(_1Kr));}),_1Kt=function(_1Ku,_1Kv){var _1Kw=E(_1Ku);return _1Kw[0]==0?function(_ay){return new F(function(){return _2J(new T(function(){var _1Kx=B(_1CP(E(_1Kv)[1])),_1Ky=jsShow(B(_IM(_1Kx[1],_1Kx[2]))[1]),_1Kz=_1Ky;return fromJSStr(_1Kz);}),_ay);});}:function(_ay){return new F(function(){return _2J(new T(function(){var _1KA=E(E(_1Kw[1])[1]);if(!_1KA){var _1KB=jsRound(E(_1Kv)[1]),_1KC=_1KB,_1KD=jsShow(_1KC),_1KE=_1KD,_1KF=fromJSStr(_1KE);}else{var _1KG=B(_1CP(E(_1Kv)[1]));if(_1KA>=0){var _1KH=B(_1Ee(10,_1KA)),_1KI=jsRound(B(_IM(_1KG[1],_1KG[2]))[1]*_1KH),_1KJ=_1KI,_1KK=jsShow(_1KJ/_1KH),_1KL=_1KK,_1KM=fromJSStr(_1KL);}else{var _1KM=E(_1Ks);}var _1KN=_1KM,_1KO=_1KN,_1KP=_1KO,_1KQ=_1KP,_1KF=_1KQ;}var _1KR=_1KF;return _1KR;}),_ay);});};},_1KS=function(_1KT){return [0,B(_169(E(_1KT)[1]))];},_1KU=function(_1KV,_1KW,_1KX){var _1KY=E(_1KX);switch(_1KY[0]){case 3:var _1KZ=_1KY[1],_1L0=u_iswupper(_1KV),_1L1=_1L0;switch(B(_1Ke(_1KV))){case 101:var _1L2=B(_1IM(_1E5,_1xk,_1xP,_1xN,_1xU,_1xJ,_1y0,_1xW,_1y4,_1Is,new T(function(){var _1L3=E(_1KW);return _1L3[1]>=0?[1,_1L3]:[0];}),_1KZ));break;case 102:var _1L2=B(_1IM(_1E5,_1xk,_1xP,_1xN,_1xU,_1xJ,_1y0,_1xW,_1y4,_1It,new T(function(){var _1L4=E(_1KW);return _1L4[1]>=0?[1,_1L4]:[0];}),_1KZ));break;case 103:var _1L5=E(_1KW),_1L2=_1L5[1]>=0?B(A(_1Kt,[[1,_1L5],_1KZ,_1g])):B(A(_1Kt,[_5h,_1KZ,_1g]));break;default:var _1L2=E(_1Kq);}var _1L6=_1L2,_1L7=E(_1L1);if(!_1L7){var _1L8=E(_1L6);if(!_1L8[0]){return [0,_1g,_1g];}else{var _1L9=_1L8[1],_1La=_1L8[2],_1Lb=E(_1L9),_1Lc=_1Lb[1],_1Ld=E(_1Lc);return _1Ld==45?[0,_1Kp,_1La]:[0,_1g,_1L8];}}else{var _1Le=B(_8A(_1KS,_1L6));if(!_1Le[0]){return [0,_1g,_1g];}else{var _1Lf=_1Le[1],_1Lg=_1Le[2],_1Lh=E(_1Lf),_1Li=_1Lh[1],_1Lj=E(_1Li);return _1Lj==45?[0,_1Kp,_1Lg]:[0,_1g,_1Le];}}break;case 4:var _1Lk=_1KY[1],_1Ll=u_iswupper(_1KV),_1Lm=_1Ll;switch(B(_1Ke(_1KV))){case 101:var _1Ln=B(_1IM(_1C6,_1vZ,_1xs,_1xp,_1xv,_1xl,_1xB,_1xx,_1xF,_1Is,new T(function(){var _1Lo=E(_1KW);return _1Lo[1]>=0?[1,_1Lo]:[0];}),_1Lk));break;case 102:var _1Ln=B(_1IM(_1C6,_1vZ,_1xs,_1xp,_1xv,_1xl,_1xB,_1xx,_1xF,_1It,new T(function(){var _1Lp=E(_1KW);return _1Lp[1]>=0?[1,_1Lp]:[0];}),_1Lk));break;case 103:var _1Lq=E(_1KW)[1];if(_1Lq>=0){var _1Lr=E(_1Lq);if(!_1Lr){var _1Ls=jsRound(E(_1Lk)[1]),_1Lt=_1Ls,_1Lu=jsShow(_1Lt),_1Lv=_1Lu,_1Lw=fromJSStr(_1Lv);}else{var _1Lx=B(_1Ee(10,_1Lr)),_1Ly=jsRound(E(_1Lk)[1]*_1Lx),_1Lz=_1Ly,_1LA=jsShow(_1Lz/_1Lx),_1LB=_1LA,_1Lw=fromJSStr(_1LB);}var _1LC=_1Lw;}else{var _1LD=jsShow(E(_1Lk)[1]),_1LE=_1LD,_1LC=fromJSStr(_1LE);}var _1LF=_1LC,_1LG=_1LF,_1Ln=_1LG;break;default:var _1Ln=E(_1Kn);}var _1LH=_1Ln,_1LI=E(_1Lm);if(!_1LI){var _1LJ=E(_1LH);if(!_1LJ[0]){return [0,_1g,_1g];}else{var _1LK=_1LJ[1],_1LL=_1LJ[2],_1LM=E(_1LK),_1LN=_1LM[1],_1LO=E(_1LN);return _1LO==45?[0,_1Kp,_1LL]:[0,_1g,_1LJ];}}else{var _1LP=B(_8A(_1KS,_1LH));if(!_1LP[0]){return [0,_1g,_1g];}else{var _1LQ=_1LP[1],_1LR=_1LP[2],_1LS=E(_1LQ),_1LT=_1LS[1],_1LU=E(_1LT);return _1LU==45?[0,_1Kp,_1LR]:[0,_1g,_1LP];}}break;default:return E(_1Kl);}},_1LV=[0,0],_1LW=function(_1LX){while(1){var _1LY=E(_1LX);if(!_1LY[0]){_1LX=[1,I_fromInt(_1LY[1])];continue;}else{return new F(function(){return I_toString(_1LY[1]);});}}},_1LZ=function(_1M0,_1M1){return new F(function(){return _2J(fromJSStr(B(_1LW(_1M0))),_1M1);});},_1M2=[0,0],_1M3=function(_1M4,_1M5,_1M6){return _1M4<=6?B(_1LZ(_1M5,_1M6)):!B(_4d(_1M5,_1M2))?B(_1LZ(_1M5,_1M6)):[1,_5o,new T(function(){return B(_2J(fromJSStr(B(_1LW(_1M5))),[1,_5n,_1M6]));})];},_1M7=function(_1M8){return new F(function(){return _1M3(0,_1M8,_1g);});},_1M9=[0,48],_1Ma=function(_1Mb,_1Mc){var _1Md=_1Mb-B(_IP(_1Mc,0))|0;if(_1Md>0){var _1Me=function(_1Mf){return _1Mf>1?[1,_1M9,new T(function(){return B(_1Me(_1Mf-1|0));})]:E([1,_1M9,_1Mc]);};return new F(function(){return _1Me(_1Md);});}else{return E(_1Mc);}},_1Mg=[0,0],_1Mh=[0,-2147483648],_1Mi=function(_1Mj,_1Mk){while(1){var _1Ml=(function(_1Mm,_1Mn){var _1Mo=E(_1Mn);switch(_1Mo[0]){case 0:_1Mj=_1Mg;_1Mk=[2,_1Mh,new T(function(){return B(_qN(E(_1Mo[1])[1]));})];return null;case 2:var _1Mp=_1Mo[2];return !B(_4d(_1Mp,_1LV))?[0,_1g,new T(function(){return B(_1Ma(E(_1Mm)[1],B(_1M7(_1Mp))));})]:[0,_1Kp,new T(function(){return B(_1Ma(E(_1Mm)[1],B(_1M3(0,B(_qH(_1Mp)),_1g))));})];default:return E(_1Kl);}})(_1Mj,_1Mk);if(_1Ml!=null){return _1Ml;}}},_1Mq=[1,_Hm,_1g],_1Mr=function(_1Ms){return new F(function(){return err(B(unAppCStr("Char.digitToInt: not a digit ",new T(function(){var _1Mt=E(_1Ms);return _1Mt==39?E(_Ho):[1,_Hm,new T(function(){return B(_H6(_1Mt,_1Mq));})];}))));});},_1Mu=function(_1Mv){var _1Mw=function(_1Mx){var _1My=function(_1Mz){if(_1Mv<65){return new F(function(){return _1Mr(_1Mv);});}else{if(_1Mv>70){return new F(function(){return _1Mr(_1Mv);});}else{return (_1Mv-65|0)+10|0;}}};if(_1Mv<97){return new F(function(){return _1My(_);});}else{if(_1Mv>102){return new F(function(){return _1My(_);});}else{return (_1Mv-97|0)+10|0;}}};if(_1Mv<48){return new F(function(){return _1Mw(_);});}else{if(_1Mv>57){return new F(function(){return _1Mw(_);});}else{return _1Mv-48|0;}}},_1MA=function(_1MB,_1MC){while(1){var _1MD=(function(_1ME,_1MF){var _1MG=E(_1MF);if(!_1MG[0]){return [0,_1ME,_1g];}else{var _1MH=E(_1MG[1])[1];if(_1MH<48){return [0,_1ME,_1MG];}else{if(_1MH>57){return [0,_1ME,_1MG];}else{_1MB=new T(function(){return [0,(imul(E(_1ME)[1],10)|0)+B(_1Mu(_1MH))|0];});_1MC=_1MG[2];return null;}}}})(_1MB,_1MC);if(_1MD!=null){return _1MD;}}},_1MI=new T(function(){return B(unCStr("argument list ended prematurely"));}),_1MJ=new T(function(){return B(_1Ki(_1MI));}),_1MK=[0,-1],_1ML=function(_1MM){return [0,E(_1MM)[1]];},_1MN=function(_1MO){var _1MP=E(_1MO);switch(_1MP[0]){case 0:return new F(function(){return _1ML(_1MP[1]);});break;case 2:return new F(function(){return _Rm(_1MP[2]);});break;default:return E(_1Kl);}},_1MQ=function(_1MR,_1MS,_1MT,_1MU,_1MV){while(1){var _1MW=(function(_1MX,_1MY,_1MZ,_1N0,_1N1){var _1N2=E(_1N0);if(!_1N2[0]){return [0,_1Mg,_1MK,_1MX,_1MY,_1MZ,_1g,_1N1];}else{var _1N3=_1N2[2],_1N4=E(E(_1N2[1])[1]);switch(_1N4){case 42:var _1N5=new T(function(){var _1N6=E(_1N1);return _1N6[0]==0?E(_1MJ):[0,_1N6[2],new T(function(){return B(_1MN(_1N6[1]));})];}),_1N7=new T(function(){var _1N8=E(_1N3);if(!_1N8[0]){var _1N9=[0,_1MK,_1g,new T(function(){return E(E(_1N5)[1]);})];}else{if(E(E(_1N8[1])[1])==46){var _1Na=E(_1N8[2]);if(!_1Na[0]){var _1Nb=B(_1MA(_1Mg,_1g)),_1Nc=[0,_1Nb[1],_1Nb[2],new T(function(){return E(E(_1N5)[1]);})];}else{if(E(E(_1Na[1])[1])==42){var _1Nd=new T(function(){var _1Ne=E(E(_1N5)[1]);return _1Ne[0]==0?E(_1MJ):[0,_1Ne[2],new T(function(){return B(_1MN(_1Ne[1]));})];}),_1Nf=[0,new T(function(){return E(E(_1Nd)[2]);}),_1Na[2],new T(function(){return E(E(_1Nd)[1]);})];}else{var _1Ng=B(_1MA(_1Mg,_1Na)),_1Nf=[0,_1Ng[1],_1Ng[2],new T(function(){return E(E(_1N5)[1]);})];}var _1Nh=_1Nf,_1Nc=_1Nh;}var _1Ni=_1Nc;}else{var _1Ni=[0,_1MK,_1N8,new T(function(){return E(E(_1N5)[1]);})];}var _1Nj=_1Ni,_1N9=_1Nj;}return _1N9;});return [0,new T(function(){return E(E(_1N5)[2]);}),new T(function(){return E(E(_1N7)[1]);}),_1MX,_1MY,_1MZ,new T(function(){return E(E(_1N7)[2]);}),new T(function(){return E(E(_1N7)[3]);})];case 43:var _1Nk=_1MX,_1Nl=_1MY;_1MT=_gw;_1MU=_1N3;var _1Nm=_1N1;_1MR=_1Nk;_1MS=_1Nl;_1MV=_1Nm;return null;case 45:_1MR=_gw;var _1Nl=_1MY,_1Nn=_1MZ;_1MU=_1N3;var _1Nm=_1N1;_1MS=_1Nl;_1MT=_1Nn;_1MV=_1Nm;return null;case 46:var _1No=new T(function(){var _1Np=E(_1N3);if(!_1Np[0]){var _1Nq=B(_1MA(_1Mg,_1g)),_1Nr=[0,_1Nq[1],_1Nq[2],_1N1];}else{if(E(E(_1Np[1])[1])==42){var _1Ns=new T(function(){var _1Nt=E(_1N1);return _1Nt[0]==0?E(_1MJ):[0,_1Nt[2],new T(function(){return B(_1MN(_1Nt[1]));})];}),_1Nu=[0,new T(function(){return E(E(_1Ns)[2]);}),_1Np[2],new T(function(){return E(E(_1Ns)[1]);})];}else{var _1Nv=B(_1MA(_1Mg,_1Np)),_1Nu=[0,_1Nv[1],_1Nv[2],_1N1];}var _1Nw=_1Nu,_1Nr=_1Nw;}return _1Nr;});return [0,_1Mg,new T(function(){return E(E(_1No)[1]);}),_1MX,_1MY,_1MZ,new T(function(){return E(E(_1No)[2]);}),new T(function(){return E(E(_1No)[3]);})];case 48:var _1Nk=_1MX;_1MS=_gw;var _1Nn=_1MZ;_1MU=_1N3;var _1Nm=_1N1;_1MR=_1Nk;_1MT=_1Nn;_1MV=_1Nm;return null;default:if(_1N4<48){return [0,_1Mg,_1MK,_1MX,_1MY,_1MZ,_1N2,_1N1];}else{if(_1N4>57){return [0,_1Mg,_1MK,_1MX,_1MY,_1MZ,_1N2,_1N1];}else{var _1Nx=new T(function(){var _1Ny=B(_1MA(_1Mg,_1N2));return [0,_1Ny[1],_1Ny[2]];}),_1Nz=new T(function(){var _1NA=E(E(_1Nx)[2]);if(!_1NA[0]){var _1NB=[0,_1MK,_1g,_1N1];}else{if(E(E(_1NA[1])[1])==46){var _1NC=E(_1NA[2]);if(!_1NC[0]){var _1ND=B(_1MA(_1Mg,_1g)),_1NE=[0,_1ND[1],_1ND[2],_1N1];}else{if(E(E(_1NC[1])[1])==42){var _1NF=new T(function(){var _1NG=E(_1N1);return _1NG[0]==0?E(_1MJ):[0,_1NG[2],new T(function(){return B(_1MN(_1NG[1]));})];}),_1NH=[0,new T(function(){return E(E(_1NF)[2]);}),_1NC[2],new T(function(){return E(E(_1NF)[1]);})];}else{var _1NI=B(_1MA(_1Mg,_1NC)),_1NH=[0,_1NI[1],_1NI[2],_1N1];}var _1NJ=_1NH,_1NE=_1NJ;}var _1NK=_1NE;}else{var _1NK=[0,_1MK,_1NA,_1N1];}var _1NL=_1NK,_1NB=_1NL;}var _1NM=_1NB;return _1NM;});return [0,new T(function(){return E(E(_1Nx)[1]);}),new T(function(){return E(E(_1Nz)[1]);}),_1MX,_1MY,_1MZ,new T(function(){return E(E(_1Nz)[2]);}),new T(function(){return E(E(_1Nz)[3]);})];}}}}})(_1MR,_1MS,_1MT,_1MU,_1MV);if(_1MW!=null){return _1MW;}}},_1NN=new T(function(){return B(unCStr("formatting string ended prematurely"));}),_1NO=new T(function(){return B(_1Ki(_1NN));}),_1NP=function(_1NQ,_1NR){if(!B(_4d(_1NR,_1NQ))){if(!B(_3j(_1NQ,_1LV))){var _1NS=B(_12z(_1NR,_1NQ));return new F(function(){return _2J(B(_1NP(_1NQ,_1NS[1])),[1,new T(function(){return [0,B(_1Es(B(_s2(_1NS[2]))))];}),_1g]);});}else{return E(_3i);}}else{return [1,new T(function(){return [0,B(_1Es(B(_s2(_1NR))))];}),_1g];}},_1NT=[0,2],_1NU=function(_1NV,_1NW,_1NX){var _1NY=E(_1NX);switch(_1NY[0]){case 0:return new F(function(){return _1NP(_1NV,B(_qN(E(_1NY[1])[1])));});break;case 2:var _1NZ=_1NY[2],_1O0=E(_1NW)[1];if(!B(_4d(_1NZ,_1LV))){return new F(function(){return _1Ma(_1O0,B(_1NP(_1NV,_1NZ)));});}else{return new F(function(){return _1Ma(_1O0,B(_1NP(_1NV,B(_qx(B(_qH(B(_qP(_1NT,_1NY[1])))),_1NZ)))));});}break;default:return E(_1Kl);}},_1O1=[0,37],_1O2=[0,16],_1O3=[0,10],_1O4=[0,8],_1O5=[0,43],_1O6=[1,_1O5,_1g],_1O7=[0,32],_1O8=function(_1O9){return new F(function(){return _1Ki(new T(function(){return B(unAppCStr("bad formatting char ",[1,_1O9,_1g]));}));});},_1Oa=function(_1Ob,_1Oc){var _1Od=E(_1Ob);if(!_1Od[0]){return E(_1Oc)[0]==0?[0]:E(_1NO);}else{var _1Oe=_1Od[2],_1Of=E(_1Od[1]);if(E(_1Of[1])==37){var _1Og=function(_1Oh){var _1Oi=E(_1Oc);if(!_1Oi[0]){return E(_1MJ);}else{var _1Oj=B(_1MQ(_5E,_5E,_5E,_1Oe,_1Oi)),_1Ok=_1Oj[2],_1Ol=_1Oj[4],_1Om=E(_1Oj[6]);if(!_1Om[0]){return E(_1NO);}else{var _1On=_1Om[2],_1Oo=E(_1Oj[7]);if(!_1Oo[0]){return E(_1MJ);}else{var _1Op=_1Oo[1],_1Oq=_1Oo[2],_1Or=E(_1Om[1]),_1Os=function(_1Ot,_1Ou){var _1Ov=new T(function(){var _1Ow=B(_IP(_1Ou,0)),_1Ox=B(_IP(_1Ot,0)),_1Oy=E(_1Oj[1])[1];if((_1Ow+_1Ox|0)>=_1Oy){var _1Oz=[0];}else{var _1OA=_1Oy-(_1Ow+_1Ox|0)|0;if(_1OA>0){if(_1OA<0){var _1OB=[0];}else{var _1OC=new T(function(){return [1,new T(function(){return !E(_1Ol)?E(_1O7):E(_1M9);}),_1OC];}),_1OB=B(_Tt(_1OA,_1OC));}var _1OD=_1OB,_1OE=_1OD;}else{var _1OE=[0];}var _1OF=_1OE,_1OG=_1OF,_1OH=_1OG,_1Oz=_1OH;}var _1OI=_1Oz,_1OJ=_1OI,_1OK=_1OJ,_1OL=_1OK,_1OM=_1OL;return _1OM;},1);return !E(_1Oj[3])?!E(_1Ol)?B(_2J(_1Ov,new T(function(){return B(_2J(_1Ot,_1Ou));},1))):B(_2J(_1Ot,new T(function(){return B(_2J(_1Ov,_1Ou));},1))):B(_2J(_1Ot,new T(function(){return B(_2J(_1Ou,_1Ov));},1)));},_1ON=function(_1OO,_1OP){var _1OQ=E(_1OO);return _1OQ[0]==0?!E(_1Oj[5])?B(_1Os(_1g,_1OP)):B(_1Os(_1O6,_1OP)):B(_1Os(_1OQ,_1OP));};switch(E(_1Or[1])){case 69:var _1OR=B(_1KU(69,_1Ok,_1Op));return new F(function(){return _2J(B(_1ON(_1OR[1],_1OR[2])),new T(function(){return B(_1Oa(_1On,_1Oq));},1));});break;case 71:var _1OS=B(_1KU(71,_1Ok,_1Op));return new F(function(){return _2J(B(_1ON(_1OS[1],_1OS[2])),new T(function(){return B(_1Oa(_1On,_1Oq));},1));});break;case 88:return new F(function(){return _2J(B(_1Os(_1g,new T(function(){return B(_8A(_1KS,B(_1NU(_1O2,_1Ok,_1Op))));}))),new T(function(){return B(_1Oa(_1On,_1Oq));},1));});break;case 99:return new F(function(){return _2J(B(_1Os(_1g,[1,new T(function(){var _1OT=E(_1Op);switch(_1OT[0]){case 0:var _1OU=E(_1OT[1])[1];if(_1OU>>>0>1114111){var _1OV=B(_s0(_1OU));}else{var _1OV=[0,_1OU];}var _1OW=_1OV,_1OX=_1OW,_1OY=_1OX,_1OZ=_1OY,_1P0=_1OZ;break;case 2:var _1P1=B(_s2(_1OT[2]));if(_1P1>>>0>1114111){var _1P2=B(_s0(_1P1));}else{var _1P2=[0,_1P1];}var _1P3=_1P2,_1P4=_1P3,_1P5=_1P4,_1P0=_1P5;break;default:var _1P0=E(_1Kl);}return _1P0;}),_1g])),new T(function(){return B(_1Oa(_1On,_1Oq));},1));});break;case 100:var _1P6=B(_1Mi(_1Ok,_1Op));return new F(function(){return _2J(B(_1ON(_1P6[1],_1P6[2])),new T(function(){return B(_1Oa(_1On,_1Oq));},1));});break;case 101:var _1P7=B(_1KU(101,_1Ok,_1Op));return new F(function(){return _2J(B(_1ON(_1P7[1],_1P7[2])),new T(function(){return B(_1Oa(_1On,_1Oq));},1));});break;case 102:var _1P8=B(_1KU(102,_1Ok,_1Op));return new F(function(){return _2J(B(_1ON(_1P8[1],_1P8[2])),new T(function(){return B(_1Oa(_1On,_1Oq));},1));});break;case 103:var _1P9=B(_1KU(103,_1Ok,_1Op));return new F(function(){return _2J(B(_1ON(_1P9[1],_1P9[2])),new T(function(){return B(_1Oa(_1On,_1Oq));},1));});break;case 105:var _1Pa=B(_1Mi(_1Ok,_1Op));return new F(function(){return _2J(B(_1ON(_1Pa[1],_1Pa[2])),new T(function(){return B(_1Oa(_1On,_1Oq));},1));});break;case 111:return new F(function(){return _2J(B(_1Os(_1g,new T(function(){return B(_1NU(_1O4,_1Ok,_1Op));}))),new T(function(){return B(_1Oa(_1On,_1Oq));},1));});break;case 115:return new F(function(){return _2J(B(_1Os(_1g,new T(function(){var _1Pb=E(_1Op);if(_1Pb[0]==1){var _1Pc=_1Pb[1],_1Pd=E(_1Ok)[1];if(_1Pd<0){var _1Pe=E(_1Pc);}else{var _1Pe=_1Pd>0?B(_Tt(_1Pd,_1Pc)):[0];}var _1Pf=_1Pe,_1Pg=_1Pf,_1Ph=_1Pg;}else{var _1Ph=E(_1Kl);}return _1Ph;}))),new T(function(){return B(_1Oa(_1On,_1Oq));},1));});break;case 117:return new F(function(){return _2J(B(_1Os(_1g,new T(function(){return B(_1NU(_1O3,_1Ok,_1Op));}))),new T(function(){return B(_1Oa(_1On,_1Oq));},1));});break;case 120:return new F(function(){return _2J(B(_1Os(_1g,new T(function(){return B(_1NU(_1O2,_1Ok,_1Op));}))),new T(function(){return B(_1Oa(_1On,_1Oq));},1));});break;default:return new F(function(){return _1O8(_1Or);});}}}}},_1Pi=E(_1Oe);if(!_1Pi[0]){return new F(function(){return _1Og(_);});}else{if(E(E(_1Pi[1])[1])==37){return [1,_1O1,new T(function(){return B(_1Oa(_1Pi[2],_1Oc));})];}else{return new F(function(){return _1Og(_);});}}}else{return [1,_1Of,new T(function(){return B(_1Oa(_1Oe,_1Oc));})];}}},_1Pj=[0,48],_1Pk=[1,_1Pj,_1g],_1Pl=function(_1Pm){return _1Pm>1?[1,_1Pj,new T(function(){return B(_1Pl(_1Pm-1|0));})]:E(_1Pk);},_1Pn=function(_1Po){return _1Po>1?[1,_1Pj,new T(function(){return B(_1Pn(_1Po-1|0));})]:E(_1Pk);},_1Pp=function(_1Pq,_1Pr){var _1Ps=B(_8A(_1vc,B(_1Oa(B(unAppCStr("%.",new T(function(){return B(_2J(B(_5p(0,E(_1Pq)[1],_1g)),_1vf));}))),new T(function(){return B(_1vg([1,[4,_1Pr],_1g],_1g));},1)))));return !B(_hS(_1Ps,B(unAppCStr("-0.",new T(function(){var _1Pt=E(_1Pq)[1];return _1Pt>0?B(_1Pn(_1Pt)):[0];})))))?E(_1Ps):B(unAppCStr("0.",new T(function(){var _1Pu=E(_1Pq)[1];return _1Pu>0?B(_1Pl(_1Pu)):[0];})));},_1Pv=function(_1Pw,_1Px){var _1Py=E(_1Pw);if(!_1Py[0]){return [0];}else{var _1Pz=E(_1Px);return _1Pz[0]==0?[0]:[1,[0,_1Py[1],_1Pz[1]],new T(function(){return B(_1Pv(_1Py[2],_1Pz[2]));})];}},_1PA=function(_1PB,_1PC,_1PD,_1PE,_1PF,_1PG,_1PH,_1PI){var _1PJ=new T(function(){var _1PK=E(_1PD);if(!_1PK[0]){var _1PL=E(_1lm);}else{var _1PL=E(E(_1PK[1])[1]);}return _1PL;}),_1PM=new T(function(){var _1PN=E(_1PJ);if(!_1PN[0]){var _1PO=E(_1v9);}else{var _1PP=E(_1PN[1]),_1PQ=_1PP[1],_1PR=_1PP[2],_1PS=E(_1PN[2]);if(!_1PS[0]){var _1PT=E(_1PQ);}else{var _1PU=_1PS[2],_1PV=E(_1PQ)[1],_1PW=E(_1PS[1]),_1PX=E(_1PW[1])[1];if(_1PV>=_1PX){if(_1PV!=_1PX){var _1PY=E(B(_1sJ(_1PX,_1PW[2],_1PU))[1]);}else{var _1PY=E(B(_1sJ(_1PV,_1PR,_1PU))[1]);}var _1PZ=_1PY,_1Q0=_1PZ;}else{var _1Q0=E(B(_1sJ(_1PV,_1PR,_1PU))[1]);}var _1Q1=_1Q0,_1Q2=_1Q1,_1Q3=_1Q2,_1Q4=_1Q3,_1PT=_1Q4;}var _1Q5=_1PT,_1PO=_1Q5;}return _1PO;}),_1Q6=new T(function(){var _1Q7=E(_1PJ);if(!_1Q7[0]){var _1Q8=E(_7P);}else{var _1Q9=E(_1Q7[1]),_1Qa=_1Q9[1],_1Qb=E(_1Q7[2]);if(!_1Qb[0]){var _1Qc=E(_1Qa)[1]-E(_1PM)[1],_1Qd=_1Qc>=1.0e-3?[0,_1Qc]:E(_1v5);}else{var _1Qe=_1Qb[2],_1Qf=E(_1Qa)[1],_1Qg=E(_1Qb[1]),_1Qh=_1Qg[2],_1Qi=E(_1Qg[1])[1];if(_1Qf>=_1Qi){if(_1Qf!=_1Qi){var _1Qj=E(B(_1sR(_1Qf,_1Q9[2],_1Qe))[1])[1]-E(_1PM)[1],_1Qk=_1Qj>=1.0e-3?[0,_1Qj]:E(_1v5);}else{var _1Ql=E(B(_1sR(_1Qi,_1Qh,_1Qe))[1])[1]-E(_1PM)[1],_1Qk=_1Ql>=1.0e-3?[0,_1Ql]:E(_1v5);}var _1Qm=_1Qk,_1Qn=_1Qm;}else{var _1Qo=E(B(_1sR(_1Qi,_1Qh,_1Qe))[1])[1]-E(_1PM)[1],_1Qn=_1Qo>=1.0e-3?[0,_1Qo]:E(_1v5);}var _1Qp=_1Qn,_1Qq=_1Qp,_1Qr=_1Qq,_1Qs=_1Qr,_1Qd=_1Qs;}var _1Qt=_1Qd,_1Q8=_1Qt;}return _1Q8;}),_1Qu=new T(function(){var _1Qv=E(_1PJ);if(!_1Qv[0]){var _1Qw=E(_1v9);}else{var _1Qx=E(_1Qv[1]),_1Qy=_1Qx[1],_1Qz=_1Qx[2],_1QA=E(_1Qv[2]);if(!_1QA[0]){var _1QB=E(_1Qz);}else{var _1QC=_1QA[2],_1QD=E(_1Qz)[1],_1QE=E(_1QA[1]),_1QF=E(_1QE[2])[1];if(_1QD>=_1QF){if(_1QD!=_1QF){var _1QG=E(B(_1ss(_1QE[1],_1QF,_1QC))[2]);}else{var _1QG=E(B(_1ss(_1Qy,_1QD,_1QC))[2]);}var _1QH=_1QG,_1QI=_1QH;}else{var _1QI=E(B(_1ss(_1Qy,_1QD,_1QC))[2]);}var _1QJ=_1QI,_1QK=_1QJ,_1QL=_1QK,_1QM=_1QL,_1QB=_1QM;}var _1QN=_1QB,_1Qw=_1QN;}return _1Qw;}),_1QO=new T(function(){var _1QP=E(_1PJ);if(!_1QP[0]){var _1QQ=E(_7P);}else{var _1QR=E(_1QP[1]),_1QS=_1QR[2],_1QT=E(_1QP[2]);if(!_1QT[0]){var _1QU=E(_1QS)[1]-E(_1Qu)[1],_1QV=_1QU>=1.0e-3?[0,_1QU]:E(_1v5);}else{var _1QW=_1QT[2],_1QX=E(_1QS)[1],_1QY=E(_1QT[1]),_1QZ=_1QY[1],_1R0=E(_1QY[2])[1];if(_1QX>=_1R0){if(_1QX!=_1R0){var _1R1=E(B(_1sA(_1QR[1],_1QX,_1QW))[2])[1]-E(_1Qu)[1],_1R2=_1R1>=1.0e-3?[0,_1R1]:E(_1v5);}else{var _1R3=E(B(_1sA(_1QZ,_1R0,_1QW))[2])[1]-E(_1Qu)[1],_1R2=_1R3>=1.0e-3?[0,_1R3]:E(_1v5);}var _1R4=_1R2,_1R5=_1R4;}else{var _1R6=E(B(_1sA(_1QZ,_1R0,_1QW))[2])[1]-E(_1Qu)[1],_1R5=_1R6>=1.0e-3?[0,_1R6]:E(_1v5);}var _1R7=_1R5,_1R8=_1R7,_1R9=_1R8,_1Ra=_1R9,_1QV=_1Ra;}var _1Rb=_1QV,_1QQ=_1Rb;}return _1QQ;}),_1Rc=function(_1Rd){var _1Re=E(_1Rd);return [0,new T(function(){return [0,E(_1PE)[1]*(0.1+0.8*(E(_1Re[1])[1]-E(_1PM)[1])/E(_1Q6)[1])];}),new T(function(){return [0,E(_1PF)[1]*(1-(0.1+0.8*(E(_1Re[2])[1]-E(_1Qu)[1])/E(_1QO)[1]))];})];},_1Rf=new T(function(){return B(_8A(_1Rc,_1PJ));}),_1Rg=new T(function(){var _1Rh=E(_1PE)[1];return [0,(_1Rh+_1Rh)/900];}),_1Ri=new T(function(){return B(_1uo([1,[0,new T(function(){return [0, -(2.5e-2*E(_1PE)[1])];}),new T(function(){return [0,1.0e-2*E(_1PF)[1]];})],[1,_1v3,[1,[0,new T(function(){return [0, -(2.5e-2*E(_1PE)[1])];}),new T(function(){return [0, -(1.0e-2*E(_1PF)[1])];})],_1g]]]));});return function(_1Rj,_){var _1Rk=E(_1Rj),_1Rl=_1Rk[1],_1Rm=jsBeginPath(_1Rl),_1Rn=B(A(new T(function(){return B(_1uo([1,[0,_1v2,_1PF],[1,[0,_1PE,_1PF],_1g]]));}),[[0,_1Rl],_])),_1Ro=_1Rn,_1Rp=jsStroke(_1Rl),_1Rq=jsPushState(_1Rl),_1Rr=jsTranslate(_1Rl,E(_1PE)[1],E(_1PF)[1]),_1Rs=jsBeginPath(_1Rl),_1Rt=B(A(_1Ri,[[0,_1Rl],_])),_1Ru=_1Rt,_1Rv=jsStroke(_1Rl),_1Rw=jsPopState(_1Rl),_1Rx=jsBeginPath(_1Rl),_1Ry=B(A(new T(function(){return B(_1uo([1,[0,_1v2,_1PF],_1v4]));}),[[0,_1Rl],_])),_1Rz=_1Ry,_1RA=jsStroke(_1Rl),_1RB=jsPushState(_1Rl),_1RC=jsTranslate(_1Rl,0,0),_1RD=jsPushState(_1Rl),_1RE=jsRotate(_1Rl,-1.5707963267948966),_1RF=jsBeginPath(_1Rl),_1RG=B(A(_1Ri,[[0,_1Rl],_])),_1RH=_1RG,_1RI=jsStroke(_1Rl),_1RJ=jsPopState(_1Rl),_1RK=jsPopState(_1Rl),_1RL=jsPushState(_1Rl),_1RM=jsTranslate(_1Rl,E(new T(function(){return [0,0.8*E(_1PE)[1]];}))[1],E(new T(function(){return [0,0.95*E(_1PF)[1]];}))[1]),_1RN=E(_1Rg)[1],_1RO=jsPushState(_1Rl),_1RP=jsScale(_1Rl,_1RN,_1RN),_1RQ=jsDrawText(_1Rl,E(new T(function(){return [0,toJSStr(E(_1PB))];}))[1],0,0),_1RR=jsPopState(_1Rl),_1RS=jsPopState(_1Rl),_1RT=jsPushState(_1Rl),_1RU=jsTranslate(_1Rl,E(new T(function(){return [0,5.0e-2*E(_1PE)[1]];}))[1],0),_1RV=jsPushState(_1Rl),_1RW=jsScale(_1Rl,_1RN,_1RN),_1RX=jsDrawText(_1Rl,E(new T(function(){return [0,toJSStr(E(_1PC))];}))[1],0,0),_1RY=jsPopState(_1Rl),_1RZ=jsPopState(_1Rl),_1S0=B(A(new T(function(){var _1S1=new T(function(){return B(_IP(_1Rf,0))>2?[1,new T(function(){var _1S2=E(_1Rf);return _1S2[0]==0?E(_1lm):E(_1S2[1]);}),new T(function(){var _1S3=E(_1Rf);if(!_1S3[0]){var _1S4=E(_1oC);}else{var _1S5=B(_1ox(_1S3[1],_1S3[2])),_1S4=_1S5[0]==0?E(_1vb):E(_1S5[2]);}var _1S6=_1S4,_1S7=B(_IP(_1S6,0)),_1S8=function(_1S9){return new F(function(){return _2J(B(_1tm(_1S9,_1S6)),[1,new T(function(){var _1Sa=E(_1Rf);return _1Sa[0]==0?E(_1v0):B(_1uV(_1Sa[1],_1Sa[2]));}),_1g]);});},_1Sb=10<=_1S7,_1Sc=_1Sb,_1Sd=!_1Sc?B(_1S8(_1S7)):B(_1S8(10)),_1Se=_1Sd,_1Sf=_1Se,_1Sg=_1Sf;return _1Sg;})]:E(_1Rf);});return B(_1u3(_1v6,function(_1Sh,_){var _1Si=B(A(new T(function(){var _1Sj=function(_1Sk){var _1Sl=E(_1Sk);if(!_1Sl[0]){return E(_1ts);}else{var _1Sm=new T(function(){return E(E(_1Sl[1])[1]);});return function(_1Sn,_){var _1So=E(_1Sn),_1Sp=_1So[1],_1Sq=jsBeginPath(_1Sp),_1Sr=B(A(new T(function(){return B(_1uo([1,[0,_1Sm,_1v2],[1,[0,_1Sm,_1PF],_1g]]));}),[[0,_1Sp],_])),_1Ss=_1Sr,_1St=jsStroke(_1Sp),_1Su=jsPushState(_1Sp),_1Sv=jsTranslate(_1Sp,E(new T(function(){var _1Sw=E(_1Sm)[1],_1Sx=B(_IP(B(_1Pp(_1v7,new T(function(){return [0,E(_1PM)[1]+(_1Sw/E(_1PE)[1]-0.1)*E(_1Q6)[1]/0.8];}))),0))-1|0;if(4>_1Sx){var _1Sy=[0,_1Sw-_1Sx*5.0e-3*E(_1PE)[1]];}else{var _1Sy=[0,_1Sw-2.0e-2*E(_1PE)[1]];}var _1Sz=_1Sy,_1SA=_1Sz,_1SB=_1SA,_1SC=_1SB,_1SD=_1SC,_1SE=_1SD;return _1SE;}))[1],E(new T(function(){return [0,1.05*E(_1PF)[1]];}))[1]),_1SF=E(_1Rg)[1],_1SG=jsPushState(_1Sp),_1SH=jsScale(_1Sp,_1SF,_1SF),_1SI=jsDrawText(_1Sp,E(new T(function(){return [0,toJSStr(B(_1Pp(_1PG,new T(function(){return [0,E(_1PM)[1]+(E(_1Sm)[1]/E(_1PE)[1]-0.1)*E(_1Q6)[1]/0.8];}))))];}))[1],0,0),_1SJ=jsPopState(_1Sp),_1SK=jsPopState(_1Sp);return new F(function(){return A(new T(function(){return B(_1Sj(_1Sl[2]));}),[_1So,_]);});};}};return B(_1Sj(_1S1));}),[_1Sh,_])),_1SL=_1Si;return new F(function(){return A(new T(function(){var _1SM=function(_1SN){var _1SO=E(_1SN);if(!_1SO[0]){return E(_1ts);}else{var _1SP=new T(function(){return E(E(_1SO[1])[2]);});return function(_1SQ,_){var _1SR=E(_1SQ),_1SS=_1SR[1],_1ST=jsBeginPath(_1SS),_1SU=B(A(new T(function(){return B(_1uo([1,[0,_1v2,_1SP],[1,[0,_1PE,_1SP],_1g]]));}),[[0,_1SS],_])),_1SV=_1SU,_1SW=jsStroke(_1SS),_1SX=jsPushState(_1SS),_1SY=jsTranslate(_1SS,E(new T(function(){var _1SZ=B(_IP(B(_1Pp(_1v1,new T(function(){return [0,E(_1Qu)[1]+(1-E(_1SP)[1]/E(_1PF)[1]-0.1)*E(_1QO)[1]/0.8];}))),0))-1|0;if(4>_1SZ){var _1T0=[0, -(_1SZ*2.2e-2*E(_1PE)[1])];}else{var _1T0=[0, -(8.8e-2*E(_1PE)[1])];}var _1T1=_1T0,_1T2=_1T1,_1T3=_1T2,_1T4=_1T3,_1T5=_1T4;return _1T5;}))[1],E(new T(function(){return [0,E(_1SP)[1]+1.5e-2*E(_1PF)[1]];}))[1]),_1T6=E(_1Rg)[1],_1T7=jsPushState(_1SS),_1T8=jsScale(_1SS,_1T6,_1T6),_1T9=jsDrawText(_1SS,E(new T(function(){return [0,toJSStr(B(_1Pp(_1PH,new T(function(){return [0,E(_1Qu)[1]+(1-E(_1SP)[1]/E(_1PF)[1]-0.1)*E(_1QO)[1]/0.8];}))))];}))[1],0,0),_1Ta=jsPopState(_1SS),_1Tb=jsPopState(_1SS);return new F(function(){return A(new T(function(){return B(_1SM(_1SO[2]));}),[_1SR,_]);});};}};return B(_1SM(_1S1));}),[_1Sh,_]);});}));}),[_1Rk,_])),_1Tc=_1S0,_1Td=B(A(new T(function(){return B(_1u3(new T(function(){var _1Te=E(_1PD);if(!_1Te[0]){var _1Tf=E(_1lm);}else{var _1Tf=E(E(_1Te[1])[2]);}return _1Tf;},1),new T(function(){var _1Tg=E(_1Rf);if(!_1Tg[0]){var _1Th=E(_1ts);}else{var _1Ti=_1Tg[1];if(B(_IP(_1Tg,0))==1){var _1Tj=function(_1Tk,_){var _1Tl=E(_1Tk)[1],_1Tm=jsBeginPath(_1Tl),_1Tn=B(A(new T(function(){return B(_1uo([1,_1Ti,[1,_1Ti,_1g]]));}),[[0,_1Tl],_])),_1To=_1Tn,_1Tp=jsStroke(_1Tl);return _c;};}else{var _1Tj=B(_1uz(B(_1Pv(_1Tg,_1Tg[2]))));}var _1Th=_1Tj;}return _1Th;})));}),[_1Rk,_])),_1Tq=_1Td,_1Tr=B(A(new T(function(){var _1Ts=E(_1PD);if(!_1Ts[0]){var _1Tt=E(_1vb);}else{var _1Tu=function(_1Tv){var _1Tw=E(_1Tv);if(!_1Tw[0]){return E(_1tu);}else{var _1Tx=_1Tw[1];return function(_1Ty,_){var _1Tz=B(A(new T(function(){return B(_1u3(new T(function(){return E(E(_1Tx)[2]);},1),new T(function(){var _1TA=B(_8A(_1Rc,E(_1Tx)[1]));if(!_1TA[0]){var _1TB=E(_1ts);}else{var _1TC=_1TA[1];if(B(_IP(_1TA,0))==1){var _1TD=function(_1TE,_){var _1TF=E(_1TE)[1],_1TG=jsBeginPath(_1TF),_1TH=B(A(new T(function(){return B(_1uo([1,_1TC,[1,_1TC,_1g]]));}),[[0,_1TF],_])),_1TI=_1TH,_1TJ=jsStroke(_1TF);return _c;};}else{var _1TD=B(_1uK(B(_1Pv(_1TA,_1TA[2]))));}var _1TB=_1TD;}var _1TK=_1TB;return _1TK;})));}),[_1Ty,_])),_1TL=_1Tz,_1TM=B(A(new T(function(){return B(_1Tu(_1Tw[2]));}),[_1Ty,_])),_1TN=_1TM;return [1,_1TL,_1TN];};}},_1Tt=B(_1Tu(_1Ts[2]));}return _1Tt;}),[_1Rk,_])),_1TO=_1Tr;return new F(function(){return A(new T(function(){var _1TP=function(_1TQ){var _1TR=E(_1TQ);return _1TR[0]==0?E(_1ts):function(_1TS,_){var _1TT=B(A(new T(function(){var _1TU=E(_1TR[1]);return B(_1u3(_1TU[3],function(_1TV,_){return new F(function(){return _1t7(function(_1TW,_){return new F(function(){return _1t0(E(new T(function(){return [0,E(_1PE)[1]*(0.1+0.8*(E(_1TU[1])[1]-E(_1PM)[1])/E(_1Q6)[1])];}))[1],E(new T(function(){return [0,E(_1PF)[1]*(1-(0.1+0.8*(E(_1TU[2])[1]-E(_1Qu)[1])/E(_1QO)[1]))];}))[1],E(new T(function(){return [0,5.0e-3*E(_1PE)[1]];}))[1],E(_1TW)[1],_);});},E(_1TV)[1],_);});}));}),[_1TS,_])),_1TX=_1TT;return new F(function(){return A(new T(function(){return B(_1TP(_1TR[2]));}),[_1TS,_]);});};};return B(_1TP(_1PI));}),[_1Rk,_]);});};},_1TY=new T(function(){return B(unCStr("border: 1px solid black;"));}),_1TZ=new T(function(){return B(unCStr("canvas"));}),_1U0=function(_1U1,_1U2,_1U3,_){var _1U4=jsCreateElem(toJSStr(E(_1TZ))),_1U5=_1U4,_1U6=jsAppendChild(_1U5,E(_1U3)[1]),_1U7=[0,_1U5],_1U8=B(A(_1U1,[_1U2,_1U7,_])),_1U9=_1U8;return _1U7;},_1Ua=function(_1Ub,_){return [0,[0,_K,[1,_1Ub]],_1Ub];},_1Uc=function(_1Ud){return function(_1Ue,_){return [0,[0,_K,[1,[1,_5v,new T(function(){var _1Uf=E(_1Ud);return B(_2J(B(_5p(0,E(_1Uf[2])[1],_1g)),_1Uf[1]));})]]],_1Ue];};},_1Ug=new T(function(){return B(unCStr("canvas"));}),_1Uh=function(_1Ui,_){var _1Uj=B(_6J(_1Ua,_1Uc,_1Ui,_)),_1Uk=_1Uj;return [0,new T(function(){var _1Ul=E(E(_1Uk)[1]);return [0,_1Ul[1],new T(function(){var _1Um=E(_1Ul[2]);return _1Um[0]==0?[0]:[1,new T(function(){return B(_2J(_1Ug,_1Um[1]));})];})];}),new T(function(){return E(E(_1Uk)[2]);})];},_1Un=new T(function(){return B(unCStr("width"));}),_1Uo=new T(function(){return B(unCStr("height"));}),_1Up=[0,200,0,0],_1Uq=new T(function(){return B(unCStr("style"));}),_1Ur=function(_1Us,_1Ut,_1Uu,_1Uv,_1Uw,_1Ux,_1Uy,_1Uz,_1UA){return function(_ay,_az){return new F(function(){return _6J(_1Uh,function(_1UB,_1UC,_){return new F(function(){return _6J(_ik,function(_1UD,_1UE,_){return new F(function(){return _6J(function(_1UF,_){return [0,[0,function(_1UG,_){var _1UH=B(_1U0(_Y,_K,_1UG,_)),_1UI=_1UH,_1UJ=B(A(_d,[_t,_1UI,_5B,_1UB,_])),_1UK=_1UJ,_1UL=B(A(_d,[_t,_1UI,_1Uq,_1TY,_])),_1UM=_1UL,_1UN=B(A(_d,[_t,_1UI,_1Un,new T(function(){return B(A(_L5,[_L2,_B8,1.2*E(_1Uw)[1],_1g]));}),_])),_1UO=_1UN,_1UP=B(A(_d,[_t,_1UI,_1Uo,new T(function(){return B(A(_L5,[_L2,_B8,1.2*E(_1Ux)[1],_1g]));}),_])),_1UQ=_1UP;return _1UI;},_gx],_1UF];},function(_1UR,_1US,_){return [0,[0,function(_1UT,_){var _1UU=jsFind(toJSStr(E(_1UB))),_1UV=_1UU,_1UW=E(_1UV);if(!_1UW[0]){return _1UT;}else{var _1UX=E(_1UW[1])[1],_1UY=jsHasCtx2D(_1UX),_1UZ=_1UY;if(!E(_1UZ)){return _1UT;}else{var _1V0=jsGetCtx2D(_1UX),_1V1=_1V0,_1V2=jsResetCanvas(_1UX),_1V3=E(_1Uw),_1V4=E(_1Ux),_1V5=jsPushState(_1V1),_1V6=jsTranslate(_1V1,0.1*_1V3[1],0.1*_1V4[1]),_1V7=B(A(_1PA,[_1Uu,_1Uv,[1,[0,new T(function(){return E(E(_1Us)[1]);}),_1Up],_1Ut],_1V3,_1V4,_1Uy,_1Uz,_1UA,[0,_1V1],_])),_1V8=_1V7,_1V9=jsPopState(_1V1);return _1UT;}}},_gx],_1US];},_1UE,_);});},_1UC,_);});},_ay,_az);});};},_1Va=function(_){var _1Vb=B(A(_a,["(function() {return $(document).width();})",_])),_1Vc=_1Vb,_1Vd=B(A(_a,["(function() {return $(document).height();})",_])),_1Ve=_1Vd;return [0,new T(function(){return B(_RC(_1Vc));}),new T(function(){return B(_RC(_1Ve));})];},_1Vf=function(_1Vg,_){var _1Vh=B(_1Va(_)),_1Vi=_1Vh;return [0,[0,_K,[1,_1Vi]],_1Vg];},_1Vj=function(_1Vk){return E(E(_1Vk)[5]);},_1Vl=[0,0],_1Vm=new T(function(){return B(unCStr("\u041b\u0443\u0447\u0448\u0438\u0439 \u0444\u0438\u0442\u043d\u0435\u0441: "));}),_1Vn=new T(function(){return B(unCStr("\u041f\u043e\u043a\u043e\u043b\u0435\u043d\u0438\u0435"));}),_1Vo=new T(function(){return B(unCStr("\u0424\u0438\u0442\u043d\u0435\u0441"));}),_1Vp=[0,0],_1Vq=new T(function(){return B(unCStr("\u0422\u0435\u043a\u0443\u0449\u0438\u0439 \u0440\u0435\u0437\u0443\u043b\u044c\u0442\u0430\u0442"));}),_1Vr=[0,4],_1Vs=new T(function(){return B(unCStr("\u0422\u0435\u043a\u0443\u0449\u0438\u0439 \u043e\u0442\u0432\u0435\u0442: "));}),_1Vt=[0,200],_1Vu=new T(function(){return B(_L5(_L2,_B8,0));}),_1Vv=new T(function(){return B(A(_1Vu,[_1g]));}),_1Vw=function(_1Vx){return E(E(_1Vx)[1]);},_1Vy=[0,50,250,50],_1Vz=new T(function(){return B(unCStr("col-md-6"));}),_1VA=new T(function(){return B(unCStr("class"));}),_1VB=[0,100,100,250],_1VC=[0,2],_1VD=[0,89],_1VE=[1,_1VD,_1g],_1VF=[0,88],_1VG=[1,_1VF,_1g],_1VH=function(_1VI,_1VJ){return new F(function(){return _9f(_1VI,E(_1VJ)[1]);});},_1VK=function(_1VL,_1VM,_1VN,_1VO){return function(_1VP,_){var _1VQ=B(A(new T(function(){return B(_1Ur([0,new T(function(){var _1VR=function(_1VS){var _1VT=-10+20*_1VS/50;return [1,[0,[0,_1VT],new T(function(){return B(_9f(_1VL,_1VT));})],new T(function(){var _1VU=E(_1VS);if(_1VU==50){var _1VV=[0];}else{var _1VV=B(_1VR(_1VU+1|0));}return _1VV;})];};return B(_1VR(0));})],[1,[0,new T(function(){var _1VW=function(_1VX){return [1,[0,[0,-10+20*_1VX/4],new T(function(){return E(E(_1VL)[3]);})],new T(function(){var _1VY=E(_1VX);if(_1VY==4){var _1VZ=[0];}else{var _1VZ=B(_1VW(_1VY+1|0));}return _1VZ;})];};return B(_1VW(0));}),_1Vy],_1g],_1VG,_1VE,new T(function(){return [0,0.4*E(_1VM)[1]];}),new T(function(){return [0,E(_1VN)[1]*0.4];}),_1VC,_1VC,new T(function(){var _1W0=E(_1VO);if(!_1W0[0]){var _1W1=[0];}else{var _1W2=_1W0[1],_1W1=[1,[0,_1W2,new T(function(){return B(_1VH(_1VL,_1W2));}),_1VB],_1g];}return _1W1;})));}),[_1VP,_])),_1W3=_1VQ,_1W4=E(_1W3),_1W5=E(_1W4[1]);return [0,[0,function(_1W6,_){var _1W7=B(_bw(_Y,_1W5[1],_1W6,_)),_1W8=_1W7,_1W9=B(A(_d,[_t,_1W8,_1VA,_1Vz,_])),_1Wa=_1W9;return _1W8;},_1W5[2]],_1W4[2]];};},_1Wb=function(_1Wc,_){return [0,[0,_K,[1,_1Wc]],_1Wc];},_1Wd=function(_1We,_1Wf,_){return [0,[0,_K,[1,new T(function(){return E(E(_1We)[4]);})]],_1Wf];},_1Wg=function(_cM,_){return new F(function(){return _6J(_1Wb,_1Wd,_cM,_);});},_1Wh=function(_1Wi,_){var _1Wj=rMV(E(_9V)[1]),_1Wk=_1Wj;return [0,[0,_K,[1,_1Wk]],_1Wi];},_1Wl=[1,_c],_1Wm=[0,_K,_1Wl],_1Wn=function(_1Wo,_){return [0,_1Wm,_1Wo];},_1Wp=[0,_K,_5h],_1Wq=function(_1Wr,_){return [0,_1Wp,_1Wr];},_1Ws=function(_1Wt,_1Wu){while(1){var _1Wv=(function(_1Ww,_1Wx){var _1Wy=E(_1Wx);if(!_1Wy[0]){return [0];}else{var _1Wz=_1Wy[1],_1WA=_1Wy[2];if(!B(A(_1Ww,[_1Wz]))){var _1WB=_1Ww;_1Wu=_1WA;_1Wt=_1WB;return null;}else{return [1,_1Wz,new T(function(){return B(_1Ws(_1Ww,_1WA));})];}}})(_1Wt,_1Wu);if(_1Wv!=null){return _1Wv;}}},_1WC=function(_1WD,_1WE,_1WF,_){return new F(function(){return _6J(_gI,function(_1WG,_1WH,_){return new F(function(){return _6J(function(_1WI,_){var _1WJ=E(_9V)[1],_1WK=rMV(_1WJ),_1WL=_1WK,_=wMV(_1WJ,[1,_1WG,_1WL]);return [0,_1Wm,_1WI];},function(_1WM,_cM,_){return new F(function(){return (function(_cM,_){return new F(function(){return _6J(_1Wg,function(_1WN,_1WO,_){return new F(function(){return _6J(function(_1WP,_){var _1WQ=rMV(E(_9V)[1]),_1WR=_1WQ;return [0,[0,_K,[1,new T(function(){return B(_rw(_p8,_1WG,_1WR));})]],_1WP];},function(_1WS,_1WT,_){return new F(function(){return _6J(new T(function(){return !E(_1WS)?E(_1Wn):function(_1WU,_){var _1WV=jsSetTimeout(E(_1WD)[1],function(_){var _1WW=E(_9V)[1],_1WX=rMV(_1WW),_1WY=_1WX;if(!B(_rw(_p8,_1WG,_1WY))){return _c;}else{var _1WZ=rMV(_1WW),_1X0=_1WZ,_=wMV(_1WW,new T(function(){return B(_1Ws(function(_1X1){return new F(function(){return _p5(_1X1,_1WG);});},_1X0));})),_1X2=E(_1WN),_1X3=B(A(_1X2[1],[_])),_1X4=_1X3,_1X5=E(_1X4);if(!_1X5[0]){return _c;}else{var _1X6=B(A(_1X2[2],[_1X5[1],_])),_1X7=_1X6;return _c;}}});return [0,_1Wm,_1WU];};}),function(_1X8,_1X9,_){return new F(function(){return _6J(function(_cM,_){return new F(function(){return _6J(_1Wh,function(_1Xa){return !B(_rw(_p8,_1WG,_1Xa))?E(_1Wn):E(_1Wq);},_cM,_);});},function(_1Xb){return E(_1WE);},_1X9,_);});},_1WT,_);});},_1WO,_);});},_cM,_);});})(_cM,_);});},_1WH,_);});},_1WF,_);});},_1Xc=function(_1Xd,_1Xe,_1Xf,_1Xg){var _1Xh=new T(function(){var _1Xi=B(_9A(_1Xd,E(_1Xe)[3]));return _1Xi[0]==0?[0]:[1,new T(function(){return B(_1Vw(_1Xi[1]));})];}),_1Xj=new T(function(){var _1Xk=E(_1Xe);if(!E(_1Xk[3])[0]){var _1Xl=E(_1Xf);}else{var _1Xl=[0,new T(function(){return B(_2J(E(_1Xf)[1],[1,[0,new T(function(){return [0,E(_1Xk[2])[1]];}),new T(function(){var _1Xm=E(_1Xk[4]);if(!_1Xm[0]){var _1Xn=E(_1Vp);}else{var _1Xn=E(E(_1Xm[1])[1]);}return _1Xn;})],_1g]));})];}var _1Xo=_1Xl;return _1Xo;});return function(_ay,_az){return new F(function(){return _6J(_1Vf,function(_1Xp,_1Xq,_){var _1Xr=E(_1Xp),_1Xs=_1Xr[1],_1Xt=_1Xr[2];return new F(function(){return _6J(function(_1Xu,_){var _1Xv=B(_6J(new T(function(){return B(_1Ur(_1Xj,_1g,_1Vn,_1Vo,new T(function(){return [0,0.4*E(_1Xs)[1]];}),new T(function(){return [0,E(_1Xt)[1]*0.4];}),_1Vl,_1VC,_1g));}),function(_1Xw,_1Xx,_){return [0,[0,new T(function(){return B(_dl(_1Vq,function(_1Xy,_){return new F(function(){return _cr([1,new T(function(){return B(_cD(_1Vr,_1Vm,new T(function(){var _1Xz=E(E(_1Xe)[4]);if(!_1Xz[0]){var _1XA=E(_1Vv);}else{var _1XA=B(A(_L5,[_L2,_B8,E(E(_1Xz[1])[1])[1],_1g]));}var _1XB=_1XA;return _1XB;},1)));}),[1,new T(function(){return B(_cD(_1Vr,_1Vs,new T(function(){var _1XC=E(_1Xh);return _1XC[0]==0?[0]:B(_Lg(_1XC[1]));},1)));}),_1g]],_1Xy,_);});}));}),_gx],_1Xx];},_1Xu,_)),_1XD=_1Xv,_1XE=E(_1XD),_1XF=E(_1XE[1]);return [0,[0,function(_1XG,_){var _1XH=B(_bw(_Y,_1XF[1],_1XG,_)),_1XI=_1XH,_1XJ=B(A(_d,[_t,_1XI,_1VA,_1Vz,_])),_1XK=_1XJ;return _1XI;},_1XF[2]],_1XE[2]];},function(_1XL,_1XM,_){return new F(function(){return _6J(new T(function(){return B(_1VK(_1Xd,_1Xs,_1Xt,_1Xh));}),function(_1XN){return function(_a1,_){return new F(function(){return _6J(function(_a1,_){return new F(function(){return _1WC(_1Vt,new T(function(){var _1XO=E(_1Xg);if(!_1XO[0]){var _1XP=function(_){var _1XQ=E(_1Xe);return new F(function(){return _1rL(_1nN,new T(function(){return B(_EV(_1Xd));}),function(_1XR){return new F(function(){return _9w(_1Xd,_1XR);});},new T(function(){return B(_1Vj(_1Xd));}),_1XQ[1],_1XQ[2],_1XQ[3],_1XQ[4],_1XQ[5],_);});};}else{var _1XP=E(_1XO[1]);}var _1XS=_1XP,_1XT=function(_1XU,_){var _1XV=B(A(_1XS,[_])),_1XW=_1XV;return [0,[0,_K,[1,_1XW]],_1XU];},_1XX=E(_1XT);return _1XX;}),_a1,_);});},function(_1XY,_1XZ,_){return new F(function(){return _6J(new T(function(){var _1Y0=E(_1XY);if(!_1Y0[0]){var _1Y1=function(_1Y2,_){return [0,[0,_K,[1,[0,_1Xe,[1,E(_1Y0[1])[2]]]]],_1Y2];};}else{var _1Y1=function(_1Y3,_){return [0,[0,_K,[1,[0,_1Y0[1],_5h]]],_1Y3];};}return _1Y1;}),function(_1Y4,_1Y5,_){var _1Y6=E(_1Y4);return [0,[0,_K,[1,[0,_1Y6[1],_1Xj,_1Y6[2]]]],_1Y5];},_1XZ,_);});},_a1,_);});};},_1XM,_);});},_1Xq,_);});},_ay,_az);});};},_1Y7=[0,_1g],_1Y8=new T(function(){return B(unCStr("col-md-4 col-md-offset-5"));}),_1Y9=function(_1Ya,_1Yb,_){var _1Yc=B(_bw(_Y,_1Ya,_1Yb,_)),_1Yd=_1Yc,_1Ye=B(A(_d,[_t,_1Yd,_1VA,_1Y8,_])),_1Yf=_1Ye;return _1Yd;},_1Yg=new T(function(){return B(unCStr("row"));}),_1Yh=function(_1Yi,_1Yj,_){var _1Yk=B(_bw(_Y,_1Yi,_1Yj,_)),_1Yl=_1Yk,_1Ym=B(A(_d,[_t,_1Yl,_1VA,_1Yg,_])),_1Yn=_1Ym;return _1Yl;},_1Yo=1,_1Yp=new T(function(){return B(unCStr("\u041d\u0430\u0447\u0430\u0442\u044c \u044d\u0432\u043e\u043b\u044e\u0446\u0438\u044e"));}),_1Yq=new T(function(){return B(_j3(_1Yo,_1Yp));}),_1Yr=new T(function(){return B(unCStr("btn btn-primary btn-lg"));}),_1Ys=[0,_1VA,_1Yr],_1Yt=[1,_1Ys,_1g],_1Yu=function(_1Yv,_){var _1Yw=B(A(_1Yq,[_1Yv,_])),_1Yx=_1Yw,_1Yy=E(_1Yx),_1Yz=E(_1Yy[1]);return [0,[0,function(_a1,_){return new F(function(){return _1Yh(function(_a1,_){return new F(function(){return _1Y9(new T(function(){return B(_4W(_1Yz[1],_1Yt));}),_a1,_);});},_a1,_);});},_1Yz[2]],_1Yy[2]];},_1YA=2,_1YB=new T(function(){return B(unCStr("\u041e\u0441\u0442\u0430\u043d\u043e\u0432\u0438\u0442\u044c"));}),_1YC=new T(function(){return B(_j3(_1YA,_1YB));}),_1YD=0,_1YE=new T(function(){return B(unCStr("\u041d\u0430\u0437\u0430\u0434"));}),_1YF=new T(function(){return B(_j3(_1YD,_1YE));}),_1YG=function(_1YH,_){var _1YI=B(A(_1YF,[_1YH,_])),_1YJ=_1YI,_1YK=E(_1YJ),_1YL=E(_1YK[1]),_1YM=B(A(_1YC,[_1YK[2],_])),_1YN=_1YM,_1YO=E(_1YN),_1YP=E(_1YO[1]);return [0,[0,function(_1YQ,_){var _1YR=B(A(new T(function(){return B(_4W(_1YL[1],_1Yt));}),[_1YQ,_])),_1YS=_1YR,_1YT=B(A(new T(function(){return B(_4W(_1YP[1],_1Yt));}),[_1YQ,_])),_1YU=_1YT;return _1YQ;},new T(function(){var _1YV=E(_1YL[2]);return _1YV[0]==0?E(_1YP[2]):E(_1YV);})],_1YO[2]];},_1YW=function(_1YX,_){var _1YY=B(_1YG(_1YX,_)),_1YZ=_1YY,_1Z0=E(_1YZ),_1Z1=E(_1Z0[1]);return [0,[0,function(_a1,_){return new F(function(){return _1Yh(function(_a1,_){return new F(function(){return _1Y9(_1Z1[1],_a1,_);});},_a1,_);});},_1Z1[2]],_1Z0[2]];},_1Z2=new T(function(){return B(unCStr("\u041f\u0435\u0440\u0435\u0440\u0430\u0441\u0447\u0438\u0442\u0430\u0442\u044c"));}),_1Z3=new T(function(){return B(_j3(_1Yo,_1Z2));}),_1Z4=new T(function(){return B(unCStr("\u041d\u0430\u0447\u0430\u0442\u044c \u0441 \u043d\u0430\u0447\u0430\u043b\u0430"));}),_1Z5=new T(function(){return B(_j3(_1YD,_1Z4));}),_1Z6=function(_1Z7,_){var _1Z8=B(A(_1Z5,[_1Z7,_])),_1Z9=_1Z8,_1Za=E(_1Z9),_1Zb=E(_1Za[1]),_1Zc=B(A(_1Z3,[_1Za[2],_])),_1Zd=_1Zc,_1Ze=E(_1Zd),_1Zf=E(_1Ze[1]);return [0,[0,function(_1Zg,_){var _1Zh=B(A(new T(function(){return B(_4W(_1Zb[1],_1Yt));}),[_1Zg,_])),_1Zi=_1Zh,_1Zj=B(A(new T(function(){return B(_4W(_1Zf[1],_1Yt));}),[_1Zg,_])),_1Zk=_1Zj;return _1Zg;},new T(function(){var _1Zl=E(_1Zb[2]);return _1Zl[0]==0?E(_1Zf[2]):E(_1Zl);})],_1Ze[2]];},_1Zm=function(_1Zn,_){var _1Zo=B(_1Z6(_1Zn,_)),_1Zp=_1Zo,_1Zq=E(_1Zp),_1Zr=E(_1Zq[1]);return [0,[0,function(_a1,_){return new F(function(){return _1Yh(function(_a1,_){return new F(function(){return _1Y9(_1Zr[1],_a1,_);});},_a1,_);});},_1Zr[2]],_1Zq[2]];},_1Zs=new T(function(){return B(unAppCStr("invalid route in show state ",_7w));}),_1Zt=function(_a1,_){return new F(function(){return _0(_1Zs,_a1,_);});},_1Zu=new T(function(){return B(_5a(_1Zt));}),_1Zv=[0,_1Zu,_5h],_1Zw=new T(function(){return B(unCStr("row-fluid"));}),_1Zx=new T(function(){return B(unCStr("\u041e\u0448\u0438\u0431\u043a\u0430 \u0440\u0435\u0448\u0435\u043d\u0438\u044f: "));}),_1Zy=new T(function(){return B(unCStr("\u0420\u0435\u0437\u0443\u043b\u044c\u0442\u0430\u0442\u044b \u044d\u0432\u043e\u043b\u044e\u0446\u0438\u0438"));}),_1Zz=new T(function(){return B(unCStr("\u041b\u0443\u0447\u0448\u0435\u0435 \u0440\u0435\u0448\u0435\u043d\u0438\u0435: "));}),_1ZA=new T(function(){return B(unCStr("\u0412\u0445\u043e\u0434\u043d\u044b\u0435 \u0434\u0430\u043d\u043d\u044b\u0435"));}),_1ZB=new T(function(){return B(unCStr("\u041a\u043e\u043b\u0438\u0447\u0435\u0441\u0442\u0432\u043e \u0431\u0438\u0442\u043e\u0432:"));}),_1ZC=new T(function(){return B(unCStr("\u041a\u043e\u043b\u0438\u0447\u0435\u0441\u0442\u0432\u043e \u0431\u0438\u0442\u043e\u0432 \u0434\u043e \u0437\u0430\u043f\u044f\u0442\u043e\u0439:"));}),_1ZD=new T(function(){return B(unCStr("\u041e\u0436\u0438\u0434\u0430\u0435\u043c\u043e\u0435 \u0437\u043d\u0430\u0447\u0435\u043d\u0438\u0435:"));}),_1ZE=new T(function(){return B(unCStr("\u041d\u0430\u0441\u0442\u0440\u043e\u0439\u043a\u0438 \u044d\u0432\u043e\u043b\u044e\u0446\u0438\u0438"));}),_1ZF=new T(function(){return B(unCStr("\u0428\u0430\u043d\u0441 \u043c\u0443\u0442\u0430\u0446\u0438\u0438: "));}),_1ZG=new T(function(){return B(unCStr("\u0427\u0430\u0441\u0442\u044c \u044d\u043b\u0438\u0442\u044b: "));}),_1ZH=new T(function(){return B(unCStr("\u041c\u0430\u043a\u0441\u0438\u043c\u0430\u043b\u044c\u043d\u043e\u0435 \u0447\u0438\u0441\u043b\u043e \u043f\u043e\u043a\u043e\u043b\u0435\u043d\u0438\u0439: "));}),_1ZI=new T(function(){return B(unCStr("\u041a\u043e\u043b-\u0432\u043e \u043f\u043e\u043f\u0443\u043b\u044f\u0446\u0438\u0439: "));}),_1ZJ=new T(function(){return B(unCStr("\u041a\u043e\u043b-\u0432\u043e \u0438\u043d\u0434\u0438\u0432\u0438\u0434\u043e\u0432 \u0432 \u043f\u043e\u043f\u0443\u043b\u044f\u0446\u0438\u0438: "));}),_1ZK=new T(function(){return B(unCStr("\u041e\u0436\u0438\u0434\u0430\u0435\u043c\u043e\u0435 \u0437\u043d\u0430\u0447\u0435\u043d\u0438\u0435 \u0444\u0438\u0442\u043d\u0435\u0441\u0430: "));}),_1ZL=[0,_K,_5h],_1ZM=function(_1ZN,_1ZO,_){return [0,_1ZL,_1ZO];},_1ZP=new T(function(){return B(unCStr("\u0414\u0440\u0443\u0433\u0430\u044f \u0438\u043d\u0444\u043e\u0440\u043c\u0430\u0446\u0438\u044f"));}),_1ZQ=[0,6],_1ZR=function(_1ZS,_1ZT,_1ZU){return function(_ay,_az){return new F(function(){return _6J(_1Vf,function(_1ZV,_1ZW,_){var _1ZX=E(_1ZV),_1ZY=_1ZX[1],_1ZZ=_1ZX[2];return new F(function(){return _6J(function(_200,_){var _201=B(_6J(function(_202,_){var _203=B(A(new T(function(){return B(_1VK(_1ZS,_1ZY,_1ZZ,[1,new T(function(){return E(E(_1ZU)[1]);})]));}),[_202,_])),_204=_203,_205=E(_204),_206=E(_205[1]);return [0,[0,function(_207,_){var _208=B(_bw(_Y,_206[1],_207,_)),_209=_208,_20a=B(A(_d,[_t,_209,_1VA,_1Vz,_])),_20b=_20a;return _209;},_206[2]],_205[2]];},function(_20c,_20d,_){var _20e=B(A(new T(function(){return B(_1Ur(_1ZT,_1g,_1Vn,_1Vo,new T(function(){return [0,E(_1ZY)[1]*0.4];}),new T(function(){return [0,E(_1ZZ)[1]*0.4];}),_1Vl,_1VC,_1g));}),[_20d,_])),_20f=_20e,_20g=E(_20f),_20h=E(_20g[1]);return [0,[0,function(_20i,_){var _20j=B(_bw(_Y,_20h[1],_20i,_)),_20k=_20j,_20l=B(A(_d,[_t,_20k,_1VA,_1Vz,_])),_20m=_20l;return _20k;},_20h[2]],_20g[2]];},_200,_)),_20n=_201,_20o=E(_20n),_20p=E(_20o[1]);return [0,[0,function(_20q,_){var _20r=B(_bw(_Y,_20p[1],_20q,_)),_20s=_20r,_20t=B(A(_d,[_t,_20s,_1VA,_1Yg,_])),_20u=_20t;return _20s;},_20p[2]],_20o[2]];},function(_20v,_20w,_){var _20x=B(_6J(function(_20y,_){return [0,[0,function(_20z,_){var _20A=B(_bw(_Y,function(_20B,_){return new F(function(){return _cr([1,function(_20C,_){var _20D=B(_bw(_Y,new T(function(){return B(_dl(_1ZA,function(_20E,_){return new F(function(){return _cr([1,new T(function(){return B(_cD(_1ZQ,_1ZB,new T(function(){return B(_5p(0,E(E(_1ZS)[2])[1],_1g));},1)));}),[1,new T(function(){return B(_cD(_1ZQ,_1ZC,new T(function(){return B(_5p(0,E(E(_1ZS)[1])[1],_1g));},1)));}),[1,new T(function(){return B(_cD(_1ZQ,_1ZD,new T(function(){return B(A(_L5,[_L2,_B8,E(E(_1ZS)[3])[1],_1g]));},1)));}),_1g]]],_20E,_);});}));}),_20C,_)),_20F=_20D,_20G=B(A(_d,[_t,_20F,_1VA,_1Vz,_])),_20H=_20G;return _20F;},[1,function(_20I,_){var _20J=B(_bw(_Y,new T(function(){var _20K=new T(function(){return E(E(_1ZS)[5]);});return B(_dl(_1ZE,function(_20L,_){return new F(function(){return _cr([1,new T(function(){return B(_cD(_1ZQ,_1ZF,new T(function(){return B(A(_L5,[_L2,_B8,E(E(_20K)[1])[1],_1g]));},1)));}),[1,new T(function(){return B(_cD(_1ZQ,_1ZG,new T(function(){return B(A(_L5,[_L2,_B8,E(E(_20K)[2])[1],_1g]));},1)));}),[1,new T(function(){return B(_cD(_1ZQ,_1ZH,new T(function(){return B(_5p(0,E(E(_20K)[3])[1],_1g));},1)));}),[1,new T(function(){return B(_cD(_1ZQ,_1ZI,new T(function(){return B(_5p(0,E(E(_20K)[4])[1],_1g));},1)));}),[1,new T(function(){return B(_cD(_1ZQ,_1ZJ,new T(function(){return B(_5p(0,E(E(_20K)[5])[1],_1g));},1)));}),[1,new T(function(){return B(_cD(_1ZQ,_1ZK,new T(function(){var _20M=E(E(_20K)[6]);return _20M[0]==0?[0]:B(_Lg(_20M[1]));},1)));}),_1g]]]]]],_20L,_);});}));}),_20I,_)),_20N=_20J,_20O=B(A(_d,[_t,_20N,_1VA,_1Vz,_])),_20P=_20O;return _20N;},[1,function(_20Q,_){var _20R=B(_bw(_Y,new T(function(){return B(_dl(_1Zy,function(_20S,_){return new F(function(){return _cr([1,new T(function(){return B(_cD(_1ZQ,_1Vm,new T(function(){return B(A(_L5,[_L2,_B8,E(E(_1ZU)[2])[1],_1g]));},1)));}),[1,new T(function(){return B(_cD(_1ZQ,_1Zz,new T(function(){return B(A(_L5,[_L2,_B8,E(E(_1ZU)[1])[1],_1g]));},1)));}),_1g]],_20S,_);});}));}),_20Q,_)),_20T=_20R,_20U=B(A(_d,[_t,_20T,_1VA,_1Vz,_])),_20V=_20U;return _20T;},[1,function(_20W,_){var _20X=B(_bw(_Y,new T(function(){return B(_dl(_1ZP,function(_20Y,_){return new F(function(){return _cr([1,new T(function(){return B(_cD(_1ZQ,_1Zx,new T(function(){return B(A(_L5,[_L2,_B8,1/E(E(_1ZU)[2])[1],_1g]));},1)));}),_1g],_20Y,_);});}));}),_20W,_)),_20Z=_20X,_210=B(A(_d,[_t,_20Z,_1VA,_1Vz,_])),_211=_210;return _20Z;},_1g]]]],_20B,_);});},_20z,_)),_212=_20A,_213=B(A(_d,[_t,_212,_1VA,_1Zw,_])),_214=_213;return _212;},_gx],_20y];},_1ZM,_20w,_)),_215=_20x,_216=E(_215),_217=E(_216[1]);return [0,[0,function(_218,_){var _219=B(_bw(_Y,_217[1],_218,_)),_21a=_219,_21b=B(A(_d,[_t,_21a,_1VA,_1Yg,_])),_21c=_21b;return _21a;},_217[2]],_216[2]];},_1ZW,_);});},_ay,_az);});};},_21d=function(_21e){var _21f=E(_21e);switch(_21f[0]){case 0:var _21g=_21f[1];return function(_ay,_az){return new F(function(){return _6J(function(_a1,_){return new F(function(){return _ac(new T(function(){return B(_Mc(_21g));}),_1Yu,_a1,_);});},function(_21h,_21i,_){var _21j=E(_21h);if(!_21j[0]){return [0,[0,_K,[1,[0,_21j[1]]]],_21i];}else{var _21k=E(_21j[1]);return _21k==1?B(_6J(_9M,function(_21l,_21m,_){return [0,[0,_K,[1,[1,_21g,_1Y7,_21l,_5h]]],_21m];},_21i,_)):[0,[0,new T(function(){return B(_5a(function(_a1,_){return new F(function(){return _0(new T(function(){return B(unAppCStr("invalid route in config state ",new T(function(){return E(_21k)==0?E(_7x):E(_7w);})));}),_a1,_);});}));}),_5h],_21i];}},_ay,_az);});};case 1:var _21n=_21f[1],_21o=_21f[2],_21p=_21f[3];return function(_ay,_az){return new F(function(){return _6J(function(_a1,_){return new F(function(){return _ac(new T(function(){return B(_1Xc(_21n,_21p,_21o,_21f[4]));}),_1YW,_a1,_);});},function(_21q,_21r,_){var _21s=E(_21q);if(!_21s[0]){var _21t=E(_21s[1]),_21u=_21t[2];return [0,[0,_K,[1,new T(function(){var _21v=E(_21t[1]);return !E(_21v[1])?[1,_21n,_21u,_21v,_21t[3]]:[2,_21n,_21u,new T(function(){var _21w=B(_9A(_21n,_21v[3]));return _21w[0]==0?E(_hD):E(_21w[1]);})];})]],_21r];}else{return new F(function(){return _6J(_9W,function(_21x){return E(new T(function(){switch(E(_21s[1])){case 0:var _21y=function(_21z,_){return [0,[0,_K,[1,[0,_21n]]],_21z];};break;case 1:var _21y=E(_a4);break;default:var _21y=function(_21A,_){return [0,[0,_K,[1,[2,_21n,_21o,new T(function(){var _21B=B(_9A(_21n,E(_21p)[3]));return _21B[0]==0?E(_hD):E(_21B[1]);})]]],_21A];};}return _21y;}));},_21r,_);});}},_ay,_az);});};default:var _21C=_21f[1];return function(_ay,_az){return new F(function(){return _6J(function(_a1,_){return new F(function(){return _ac(new T(function(){return B(_1ZR(_21C,_21f[2],_21f[3]));}),_1Zm,_a1,_);});},function(_21D,_21E,_){var _21F=E(_21D);if(!_21F[0]){return [0,[0,_K,[1,_21f]],_21E];}else{switch(E(_21F[1])){case 0:return [0,[0,_K,[1,[0,_21C]]],_21E];case 1:return new F(function(){return _6J(_9M,function(_21G,_21H,_){return [0,[0,_K,[1,[1,_21C,_1Y7,_21G,_5h]]],_21H];},_21E,_);});break;default:return [0,_1Zv,_21E];}}},_ay,_az);});};}},_21I=[0,35],_21J=new T(function(){return B(unCStr("id"));}),_21K=function(_21L,_21M,_21N,_){var _21O=B(A(_5w,[_21N,_21N,_])),_21P=_21O,_21Q=new T(function(){return E(E(_21P)[1]);}),_21R=function(_21S,_21T){return function(_ay,_az){return new F(function(){return _6J(function(_21U,_){return new F(function(){return _1z([1,_21I,_21S],_1a,new T(function(){return B(A(_21M,[_21T]));}),_21U,_);});},function(_21V){return new F(function(){return _21R(_21S,_21V);});},_ay,_az);});};},_21W=B(A(_21R,[_21Q,_21L,new T(function(){return E(E(_21P)[2]);}),_])),_21X=_21W,_21Y=E(_21X),_21Z=E(_21Y[1]);return [0,[0,function(_220,_){var _221=B(_6z(_Y,_K,_220,_)),_222=_221,_223=B(A(_d,[_t,_222,_21J,_21Q,_])),_224=_223,_225=B(A(_21Z[1],[_220,_])),_226=_225;return _220;},_21Z[2]],_21Y[2]];},_227=function(_228,_){return new F(function(){return _21K(_4G,_21d,_228,_);});},_229=[0,1000],_22a=function(_22b,_){return new F(function(){return _1WC(_229,_227,_22b,_);});},_22c=new T(function(){return B(unCStr("main-content"));}),_22d=[1,_21I,_22c],_22e=function(_22f,_){return new F(function(){return _1z(_22d,_1a,_22a,_22f,_);});},_22g=new T(function(){return B(unCStr("https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/js/bootstrap.min.js"));}),_22h=new T(function(){return B(unCStr("https://code.jquery.com/jquery-1.11.2.min.js"));}),_22i=[0,125],_22j=[1,_22i,_1g],_22k=new T(function(){return B(unAppCStr("  justify-content: center; \n",_22j));}),_22l=new T(function(){return B(unAppCStr("  align-items: center;\n",_22k));}),_22m=new T(function(){return B(unAppCStr("  display: flex;\n",_22l));}),_22n=new T(function(){return B(unAppCStr(".vertical-align > [class*=\" col-\"] {\n",_22m));}),_22o=new T(function(){return B(unAppCStr(".vertical-align > [class^=\"col-\"],\n",_22n));}),_22p=new T(function(){return B(unAppCStr("}\n",_22o));}),_22q=new T(function(){return B(unAppCStr("  flex-direction: row;\n",_22p));}),_22r=new T(function(){return B(unAppCStr("  display: flex;\n",_22q));}),_22s=new T(function(){return B(unAppCStr(".vertical-align {\n",_22r));}),_22t=new T(function(){return B(unCStr("var mouse = {x: 0, y: 0};\ndocument.addEventListener(\'mousemove\', function(e){\nmouse.x = e.clientX || e.pageX;\nmouse.y = e.clientY || e.pageY\n}, false);"));}),_22u=new T(function(){return B(unCStr("(function(){return document.body;})"));}),_22v=function(_22w,_){var _22x=B(A(_a,[toJSStr(E(_22u)),_])),_22y=_22x,_22z=E(_6a)[1],_22A=takeMVar(_22z),_22B=_22A,_22C=B(A(_22w,[_22B,_])),_22D=_22C,_22E=E(_22D),_22F=E(_22E[1]),_=putMVar(_22z,_22E[2]),_22G=B(A(_22F[1],[[0,_22y],_])),_22H=_22G;return _22F[2];},_22I=function(_){var _22J=B(_B(_19,_)),_22K=_22J,_22L=B(_B(_18,_)),_22M=_22L,_22N=toJSStr(E(_l)),_22O=B(A(_a,[_22N,_])),_22P=_22O,_22Q=B(_54(_17,[0,_22P],_)),_22R=_22Q,_22S=B(_0(_22s,_22R,_)),_22T=_22S,_22U=B(_10(_22h,_)),_22V=_22U,_22W=B(_10(_22g,_)),_22X=_22W,_22Y=B(A(_a,[_22N,_])),_22Z=_22Y,_230=B(_N(_0,_22t,[0,_22Z],_)),_231=_230;return new F(function(){return _22v(_22e,_);});},_232=function(_){return new F(function(){return _22I(_);});};
var hasteMain = function() {B(A(_232, [0]));};window.onload = hasteMain;