#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
#  arithparser.py
#  
#  Copyright 2018 Paul Alfille <paul.alfille@gmail.com>
#  
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#  
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#  
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
#  MA 02110-1301, USA.
#  
#  

import re


class CONST:
    """ Constants """
    endian = 'big'

class Error(Exception):
    """Base class for exceptions in this module."""
    pass

class ResolutionError(Error):
    """Exception raised for un defined variables.
    Allowed on first pass

    Attributes:
        expression -- input expression in which the error occurred
        variable -- name of unresolved variable"""
    

    def __init__(self, variable):
        Line.unresolved.add(variable)
        self.name = variable
        self.message = "Unresolved "+variable

class If:
    """ Manages If state
    of form .IF .ELIF .ELSE .ENDIF with only .IF and .ENDIF mandatory
    designed as a stack with lowest level ifstate active
    need 2 bits of information: current truth, and past truth history
    """
    
    def __init__(self):
        self.iflist = []
        
    def State(self):
        if self.iflist:
            return (self.iflist[0])[0]
        else:
            return True
            
    def IF(self, state):
        # Push new state
        self.iflist.insert(0,(state,False))
        
    def ELIF( self, state ):
        if self.iflist:
            (current,past) = self.iflist[0]
            if current or past:
                # must be false
                self.iflist[0] = ( False, True )
            else:
                # may be true
                self.iflist[0] = ( state, False )
        else:
            raise SyntaxError( ".ELIF without preceding .IF" )


    def ELSE(self):
        if self.iflist:
            (current,past) = self.iflist[0]
            if current or past:
                # must be false
                self.iflist[0] = ( False, True )
            else:
                # must be true
                self.iflist[0] = ( True, False )
        else:
            raise SyntaxError( ".ELSE without preceding .IF" )

    def ENDIF(self):
        if self.iflist:
            # pop out state
            self.iflist = self.iflist[1:]
        else:    
            raise SyntaxError( ".ENDIF without preceding .IF" )


class Function:
    """Function definition
    format:
    .FUNCTION funcname(ar1,arg2,..) Expression
    A function returns a value, not lines of code
    """
    function_parse = re.compile(r"^\s*\.FUNCTION\s*(?P<name>[^\s\(]*)\s*\((?P<args>[^\)]*)\)\s*(?P<expression>.*)$",flags=re.I)
    
    def __init__(self,line):
        """ looks for .FUNCTION
        Looks for f() 
        looks for expression
        """
        f = Function.function_parse.match(line)
        if f:
            self.name = f.group('name')
            self.args = [ a.strip() for a in f.group('args').split(',') if a ]
            self.expression = f.group('expression')
            #print('function',self.name,self.args,self.expression)
        else:
            raise TypeError

class Macro:
    """Macro Routines
    format:
    .MACRO macronane(ar1,arg2,..) 
    exp1
    exp2
    .ENDMACRO
    A macro creates lines of code (and local variables and labels)
    """
    macro_parse = re.compile(r"^\s*\.MACRO\s*(?P<name>\S*)\s*\((?P<args>[^\)]*)\)\s*(?P<expression>.*)$",flags=re.I)
    
    def __init__(self,line):
        """ looks for .FUNCTION
        Looks for f() 
        looks for expression
        """
        m = Macro.macro_parse.match(line)
        if m:
            self.name = m.group('name')
            self.args = [ a.strip() for a in m.group('args').split(',') if a ]
            self.expression = []
            #print("Macro",self.name,len(self.args),self.args,self.expression)
        else:
            raise TypeError
            
    def Append(self,line):
        """Append another line into the macro"""
        self.expression.append(line)


class Data:
    """ Data lines and .ORG lines """
    unspace = re.compile(r"\s+(' )")
    linespre  = re.compile(r"(([\$\%\(\[\{\+\-\*\/\^\~\&\\\<\>\,])\s+)")
    linespost = re.compile(r"(\s+([\)\]\}\+\-\*\/\^\~\&\\\<\>\,]))")    
    exquote = re.compile(r"'(.)(\S)")

    def __init__(self, line):
        """ Look at a line and return a "data" item or error"""

        plist = Data.ListSplit(line)
        #print("Data post split",plist)
        #print("Data",plist)
        if len(plist) < 2:
            raise TypeError
        elif plist[0].lower() == '.BYTE'.lower():
            self.word = 1
            qlist=[]
            for p in plist[1:]:
                qlist.append(p)
            self.data=qlist                
        elif plist[0].lower() == '.VECTOR'.lower():
            self.word = 2
            self.data = plist[1:]
        elif plist[0].lower() == '.WORD'.lower():
            self.word = 2
            self.data = plist[1:]
        else:
            raise TypeError
            
    @staticmethod
    def ListSplit( line ):
        """ Split the line using the math-protective approach"""
        # change line to remove extra spaces (within math expressions)
        li=Data.unspace.sub(r" $20 ",line)
        li = Data.linespre.sub(r"\2",li)
        li = Data.linespost.sub(r"\2",li)
        li = Data.exquote.sub(r"'\1 \2",li)
        return li.split()
        
            
    def Length(self):
        """length of this data element"""
        return len(self.data) * self.word
        
    def Render( self, varspace ):
        """ return byte array"""
        #print("Render1",self.data)
        return bytearray(0).join([varspace.EvalP( p ).to_bytes(self.word,CONST.endian) for p in self.data])

class Equ:
    """ .EQU lines """

    def __init__(self, line):
        """ Look at a line and return a "data" item or error"""

        plist = VariableSpace.Plist(line)

        if len(plist) < 3:
            raise TypeError
        elif plist[1].lower() == '.EQU'.lower():
            self.name = plist[0]
            self.expression = plist[2:]
        else:
            raise TypeError
            
    def Length(self):
        """length of this data element"""
        return 0        

class Op:
    # Class-global dict
    oplist={}
    
    # Addressing modes
    # Note relative includes direct and extended
    parse_op = re.compile(r"^\s*(?P<op>[a-zA-Z]+)\b\s*(?:(?:(?P<indexed>.*),\s*X\s*)|(?:#\s*(?P<immediate>.*))|(?:\s*(?P<relative>\S.*))|(?:.*))$")

    def __init__( self, lst ):
        """ Load a single ling containing an assembly instruction -- self documenting format"""
        self.code  = lst[0] #byte
        self.name  = lst[1] #MNEMONIC
        self.flags = lst[2] #Registers used (not used)
        self.size  = lst[3] #command plus argument length (1-3 bytes)
        self.mode  = lst[4] #addressing mode
        self.time  = lst[5] #clock cycles (not used)
        self.comment = lst[6] # explanatino (not used)
        # Load into dict
        Op.oplist[":".join([self.name,self.mode])]=self
        
    @staticmethod
    def Show( field ):
        """Show the machine language op codes"""
        if field.lower() == 'name':
            s = lambda o:(o.name,o.mode)
        elif field.lower() == 'flags':
            s = lambda o:(o.flags,o.name,o.mode)
        elif field.lower() == 'size':
            s = lambda o:(o.size,o.name,o.mode)
        elif field.lower() == 'mode':
            s = lambda o:(o.mode,o.name)
        elif field.lower() == 'time':
            s = lambda o:(o.time,o.name,o.mode)
        elif field.lower() == 'comment':
            s = lambda o:(o.comment,o.name,o.mode)
        elif field.lower() == 'code':
            s = lambda o:o.code
        else:
            print("Unrecognized sort field:",field)
            print("\tNot 'code', 'name', 'mode', 'size', 'time', 'flags' or 'comment'")
            s = lambda o:o.code
        print("M6800 Assembly Instructions")
        print("HEX Name  Mode         Flags Size Time Comment")
        for o in sorted(Op.oplist.values(),key=s):
            print(" {:02X} {:5} {:13} {:7}  {:2d} {:3d}  {}".format(o.code,o.name,o.mode,o.flags,o.size,o.time,o.comment))
    
    def Length( self ):
        return self.size
        
    @staticmethod
    def lookup( name, mode ):
        ind = ":".join([name,mode])
        if ind in Op.oplist:
            return Op.oplist[ind]
        return None
        
    @staticmethod
    def lookup_0( name ):
        """ Look up op code with no argument
        IMPLIED or ACCUMULATOR"""
        ret = Op.lookup( name, "ACCUMULATOR" )
        if ret == None:
            ret = Op.lookup(name,"IMPLIED")
        return ret
        
    @staticmethod
    def lookup_immediate(name):
        return Op.lookup(name,"IMMEDIATE")
    
    @staticmethod
    def lookup_indexed(name):
        return Op.lookup(name,"INDEXED")
    
    @staticmethod
    def lookup_16(name):
        """ 16 bit asddress
        There is also the LDX loading X from a 16 bit address"""
        return Op.lookup(name,"EXTENDED")

    @staticmethod
    def lookup_8( name ):
        """Op code  8 bit argument, not including immediate or indexed"""
        ret = Op.lookup(name,"RELATIVE")
        if ret == None:
            ret = Op.lookup(name,"DIRECT")
        return ret
        
    @staticmethod
    def ToBytes( val, size ):
        if val >= 0:
            return val.to_bytes(size,CONST.endian)
        elif size==1:
            return (val&0xFF).to_bytes(size,CONST.endian)
        elif size==2:
            return (val&0xFFFF).to_bytes(size,CONST.endian)
        else:
            raise ValueError(size)
        
    @staticmethod
    def Render( varspace, address, command ):
        """Parse the non-comment, non-label part of a line
        retuyrn asm byte sequence (op code and value)"""
        ops = Op.parse_op.match(command)
        if ops == None:
            return bytearray(0)
        else:
            opname = ops.group('op')
            indexed = ops.group('indexed')
            immediate = ops.group('immediate')
            relative = ops.group('relative')
            if indexed:
                """Indexed argument ',X' """
                val = varspace.EvalP( indexed )
                op_obj = Op.lookup_indexed(opname)
            elif immediate:
                """ Immediate argument '#' """
                val = varspace.EvalP( immediate )
                op_obj = Op.lookup_immediate(opname)
            elif relative:
                """A single value -- unadorned by '#' or ',X' -- could be 8 bit or 16"""
                val = varspace.EvalP( relative )
                op_obj = Op.lookup_8( opname )
                if op_obj and op_obj.mode == "RELATIVE":
                    # val was actual target_address -- now is relative skip after current+2
                    val = val - address - 2
                    if val > 127 or val < -128:
                        raise ValueError("Bad relative target address")
                    elif val < 0:
                        # convert to unsigned byte for conversion
                        val += 256
                else:
                    op_obj_16 = Op.lookup_16( opname )
                    if val > 256 or val < -256 or op_obj==None:
                        op_obj = op_obj_16
            else:
                """ No argument """
                op_obj = Op.lookup_0(opname)
                val = 0
        if op_obj == None:
            print(opname," not found index:",indexed," immediate:",immediate," relative:",relative)
            return bytearray(0)
        elif op_obj.size == 1:
            return op_obj.code.to_bytes(1,CONST.endian)
        elif op_obj.size == 2:
            return op_obj.code.to_bytes(1,CONST.endian)+Op.ToBytes(val,1)
        elif op_obj.size == 3:
            return op_obj.code.to_bytes(1,CONST.endian)+Op.ToBytes(val,2)
        else:
            print("bad op length",op_obj.name,op_obj.size)
            return bytearray(0)

    @staticmethod
    def ParseOp( varspace, command ):
        """Parse the non-comment, non-label part of a line
        1-st pass
        return op object or TypeError"""
        ops = Op.parse_op.match(command)
        if ops == None:
            return None
        else:
            opname = ops.group('op')
            indexed = ops.group('indexed')
            immediate = ops.group('immediate')
            relative = ops.group('relative')
            if indexed:
                """Indexed argument ',X' """
                return Op.lookup_indexed(opname)
            elif immediate:
                """ Immediate argument '#' """
                return Op.lookup_immediate(opname)
            elif relative:
                """A single value -- unadorned by '#' or ',X' -- could be 8 bit or 16"""
                op_obj_8 = Op.lookup_8( opname )
                # all relative is _8
                if op_obj_8 and op_obj_8.mode == "RELATIVE":
                    return op_obj_8
                op_obj_16 = Op.lookup_16( opname )
                # return _16 or _8 if only one exists
                if not op_obj_8:
                    return op_obj_16
                elif not op_obj_16:
                    return op_obj_8
                # both exist, need to evaluate argument 
                try:
                    val = varspace.EvalP( relative )
                except ResolutionError:
                    val = 0
                except SyntaxError:
                    return None
                op_obj_16 = Op.lookup_16( opname )
                if val > 256 or val < -256:
                    return op_obj_16
                else:
                    return op_obj_8
            else:
                """ No argument """
                return Op.lookup_0(opname)

class VariableSpace:
    """Holds valiable values and parsing information"""
    Macrospace = {}
    Functionspace = {}
    
    # number interpretation
    post_hex = re.compile(r"^\s*([0-9a-f]+)[h]\b$",flags=re.I)
    parse_hex = re.compile(r"^\s*(?:(?:\$\s*)|(?:\b0x))([0-9a-f]+)$",flags=re.I)
    parse_bin = re.compile(r"^\s*%\s*([01]+)$")
    parse_dec = re.compile(r"^\s*([0-9]+)$")
    parse_char = re.compile(r"^\s*[\'\"](.)$")
    
    linesplit = re.compile(r"(==|\bnot\]b|\bxor\b|\bor\b|\band\b|\*\*|\<\<|\>\>|\>\=|\=\>|\<\=|\=\<|[\*\+\-/\(\)\\[\]^\|\&\~\,\<\>])",flags=re.I)

    def __init__( self, parent=None ):
        #print("Creating Varspace")
        self.parent = parent
        self.local={}
        self.unused = set()
        
    def Add( self,var,val, overwrite=False ):
        """Add to local list (if exists)"""
        if var == None or var == '':
            return
        reval = self.EvalP( val )
        if var not in self.local:
            self.local[var]=reval
            self.unused.add(var)
        elif overwrite:
            # only used for .ORG label
            self.local[var]=reval
            self.unused.add(var)
        elif reval != self.local[var]:
            print("Redefine local value "+var+"= {0:6X} old= {1:6X}".format(reval,self.local[var]))
            raise ValueError("Redefine local value "+repr(var)+"="+repr(reval)+" old="+repr(self.local[var]))
            
    def Function_add( self, val ):
        var = val.name
        if var not in VariableSpace.Functionspace:
            VariableSpace.Functionspace[var]=val
        elif val != VariableSpace.Functionspace[var]:
            raise ValueError("Redefine function value")

    def Macro_add( self, macro ):
        """ Take Macro object and add to Macrospace list"""
        var = macro.name
        if var not in VariableSpace.Macrospace:
            #print("Adding macro",var, macro.name)
            VariableSpace.Macrospace[var]=macro
        else:
            raise ValueError("Redefine macro value")

    def Macro_add_line( self, macro, line ):
        macro.expression.append( line )

    def Interpret( self, var ):
        """ Parses an actual number (or hex or binary or character
        else returns None """
        n = self.parse_char.match(var)
        if n:
            #print("Interpret: <"+var+"> <"+n.group(1)+">")
            return bytes(n.group(1),'ascii')[0]
        n = self.parse_bin.match(var)
        if n:
            return int(n.group(1),2)
        n = self.parse_hex.match(var)
        if n:
            return int(n.group(1),16)
        n = self.post_hex.match(var)
        if n:
            return int(n.group(1),16)
        n = self.parse_dec.match(var)
        if n:
            return int(n.group(1),10)
        return None

    def Lookup( self, var ):
        """ Looks up a variable by name in current space, or parent space ... 
        raises ResolutionError if not found """
        #print("Lookup",var,"Parent=",self.parent)
        if var in self.local:
            #print("Found local",var,self.local[var])
            self.unused.discard(var)
            return self.local[var]
        elif self.parent:
            #print("Bumping up="+var)
            return self.parent.Lookup(var)
        else:
            print("Undefined variable name ="+var)
            raise ResolutionError(var)

    @staticmethod
    def LookupFunction( var ):
        """ returns a function object or None """
        if var in VariableSpace.Functionspace:
            return VariableSpace.Functionspace[var]
        else:
            return None

    @staticmethod
    def LookupMacro( var ):
        """ returns a macro object or None """
        if var in VariableSpace.Macrospace:
            return VariableSpace.Macrospace[var]
        else:
            return None
    
    def SingleItem( self, plist ):
        """Interpret a single item
        Either number or a variable
        Or Parenthesis
        return a tuple (value, remaining list)
        Expects to enter AT value, paren, prefix operator '+-~'
        """
        #print("Single", plist)
        if not plist:
            raise SyntaxError("Missing value in term")
        elif plist[0] == '+':
            return self.SingleItem( plist[1:])
        elif plist[0] == '-':
            ( val, qlist ) = self.SingleItem( plist[1:])
            #print("Single=",-val)
            return ( -val, qlist )
        elif plist[0] == '~':
            ( val, qlist ) = self.SingleItem( plist[1:])
            #print("Single=",~val)
            return ( ~val, qlist )
        elif plist[0].lower()=="not":
            ( val, qlist ) = self.SingleItem( plist[1:])
            if val == 0:
                return ( 1, qlist )
            else:
                return ( 0, qlist )
        elif plist[0] == '(':
            (valuelist,qlist) = self.Paren( plist[1:] )
            # unpack singleton tuple
            if len(qlist)>0 and qlist[0] == '[':
                #bracket
                ( val, qlist ) = self.Bracket( qlist[1:] )
                #print("Single bracket return",val,qlist)
                return ( valuelist[0]+val, qlist )
            else:
                #no bracket
                return (valuelist[0], qlist)
        elif plist[0] == "{":
            return self.List( plist[1:])
        elif plist[0] in r"%$'\"":
            if len(plist)>1:
                plist[1] = plist[0]+plist[1]
                return self.SingleItem( plist[1:] )
            else:
                raise SyntaxError("Bare "+plist[0])
        elif self.LookupFunction(plist[0]):
            return self.Function_apply( plist )
        else:
            # not function
            val = self.Interpret(plist[0])
            if val!=None:
                return (val, plist[1:])
            # Variable
            val = self.Lookup( plist[0] )
            if len(plist)>1 and plist[1]=='[':
                (val2,qlist) = self.Bracket( plist[2:] )
                return (val+val2, qlist)
            else:
                #print("Single=",val)
                return (val, plist[1:])

    def Power( self, plist ):
        """ Take plist and peel power series
        Highest level
        Evaluate L to R
        """
        if not plist:
            return ( None,plist )
        ( val, qlist) = self.SingleItem( plist )
        if not qlist:
            return ( val, qlist )
        if qlist[0] == "**":
            (val2,qlist) = self.Power( qlist[1:])
            if val2 == None:
                raise SyntaxError("Dangling ** operator")
            return ( val**val2, qlist)
        return ( val, qlist )
     
    def Multiplier( self, plist ):
        """ Take plist and peel mult and div series
        Below Power
        Evaluate R to L
        """
        if not plist:
            return ( None,plist )
        ( val, qlist) = self.Power( plist )
        while qlist:
            if qlist[0] == "*":
                (val2,qlist) = self.Multiplier( qlist[1:])
                if val2 == None:
                    raise SyntaxError("Dangling * operator")
                val *= val2
            elif qlist[0] == "/":
                (val2,qlist) = self.Multiplier( qlist[1:])
                if val2 == None:
                    raise SyntaxError("Dangling  operator")
                val /= val2
            else:
                break
        return ( val, qlist )

    def Shift( self, plist ):
        """ Take plist and peel shifts series
        Below multiplier
        Evaluate L to R
        """
        if not plist:
            return ( None,plist )
        ( val, qlist) = self.Multiplier( plist )
        if not qlist:
            return ( val, qlist )
        if qlist[0] == "<<":
            (val2,qlist) = self.Shift( qlist[1:])
            if val2 == None:
                raise SyntaxError("Dangling << operator")
            return ( val<<val2, qlist)
        if qlist[0] == ">>":
            (val2,qlist) = self.Shift( qlist[1:])
            if val2 == None:
                raise SyntaxError("Dangling >> operator")
            return ( val>>val2, qlist)
        return ( val, qlist )
        
    def Bitwise( self, plist ):
        """ Take plist and peel and or xor series
        Below Shift
        Evaluate R to L
        """
        if not plist:
            return ( None,plist )
        ( val, qlist) = self.Shift( plist )
        while qlist:
            if qlist[0] == "|":
                (val2,qlist) = self.Bitwise( qlist[1:])
                if val2 == None:
                    raise SyntaxError("Dangling | operator")
                val |= val2
            elif qlist[0] == "&":
                (val2,qlist) = self.Bitwise( qlist[1:])
                if val2 == None:
                    raise SyntaxError("Dangling & operator")
                val &= val2
            elif qlist[0] == "^":
                (val2,qlist) = self.Bitwise( qlist[1:])
                if val2 == None:
                    raise SyntaxError("Dangling ^ operator")
                val ^= val2
            else:
                break
        return ( val, qlist )

    def Logical( self, plist ):
        """ Take plist and peel and/or/xor series
        Below Bitwise
        Evaluate R to L
        """
        if not plist:
            return ( None,plist )
        ( val, qlist) = self.Bitwise( plist )
        while qlist:
            if qlist[0].lower() == "and":
                (val2,qlist) = self.Logical( qlist[1:])
                if val2 == None:
                    raise SyntaxError("Dangling AND operator")
                val = val and val2
            elif qlist[0].lower() == "xor":
                (val2,qlist) = self.Logical( qlist[1:])
                if val2 == None:
                    raise SyntaxError("Dangling XOR operator")
                val = (val and not val2) or (not val and val2)
            elif qlist[0].lower() == "or":
                (val2,qlist) = self.Logical( qlist[1:])
                if val2 == None:
                    raise SyntaxError("Dangling OR operator")
                val = val or val2
            else:
                break
        return ( val, qlist )

    def Terms( self, plist ):
        """ Take plist and peel addition/substraction series
        Below Bitwise
        Evaluate R to L
        """
        if not plist:
            return ( None,plist )
        ( val, qlist) = self.Logical( plist )
        while qlist:
            if qlist[0] == "+":
                (val2,qlist) = self.Terms( qlist[1:])
                if val2 == None:
                    raise SyntaxError("Dangling + operator")
                val += val2
            elif qlist[0] == "-":
                (val2,qlist) = self.Terms( qlist[1:])
                if val2 == None:
                    raise SyntaxError("Dangling & operator")
                val -= val2
            else:
                break
        return ( val, qlist )

    def Compare( self, plist ):
        """ Take plist and peel off comparison series
        Below Terms
        Binary
        """
        if not plist:
            return ( None,plist )
        ( val, qlist) = self.Terms( plist )
        if not qlist:
            return ( val, qlist )
        comp = qlist[0]
        if comp in ["<=","=<","<","==",">",">=","=>"]:
            (val2,qlist) = self.Shift( qlist[1:])
            if val2 == None:
                raise SyntaxError("Dangling "+comp+" operator")
            elif comp=="<=" or "=<":
                val = (val<=val2)
            elif comp=="<":
                val = (val<val2)
            elif comp=="==":
                val = (val==val2)
            elif comp==">":
                val = (val>val2)
            elif comp==">=" or "=>":
                val = (val>=val2)
        return ( val, qlist )

    def Paren( self, plist, num_args=1 ):
        """ Returns a 2-tuple:
        1st: list or returned elements
        2nd: plist after final ')'
        Expects to enter AFTER '(' 
        Eats final paren
        """
        #print("Paren entry", num_args,plist)
        valuelist = []
        if not plist:
            raise SyntaxError("Missing entries in parenthethesies")
        elif num_args == 0:
            #print("Null paren")
            if plist[0]==')':
                return (valuelist,plist[1:])
            raise SyntaxError("Problem with empty parameter list")
        qlist = plist
        for na in range(num_args):
            if not qlist:
                raise SyntaxError("Premature end of parenthesis expression")
            (v,qlist) = self.Compare( qlist )
            valuelist.append(v)
            if not qlist:
                raise SyntaxError("Missing delimiter in parenthesis expression")
            if na == num_args-1 and qlist[0] == ")":
                return (valuelist,qlist[1:])
            elif qlist[0] == ',':
                qlist = qlist[1:]
            else:
                break
        raise SyntaxError("Bad parenthesis expression")

    def Bracket( self, plist ):
        """ Returns a 2-tuple:
        1st: value
        2nd: plist after final ']'
        Expects to enter AFTER '[' 
        Eats final paren
        """
        #print("Bracket entry",plist)
        if not plist:
            pass
        elif plist[0]==']':
            return (0,plist[1:])
        else:
            # Note a bracket cannot return a comparison
            (val,qlist) = self.Terms( plist )
            if qlist[0] == ']':
                return ( val, qlist[1:] )
        raise SyntaxError("No closing bracket")

    def List( self, plist ):
        """ Returns a {} list:
        Expects to enter AFTER '{' 
        Eats final } and all commas
        """
        #print("List", num_args,plist)
        val = []
        while len(plist)>0:
            (v,qlist) = self.Compare( qlist[1:])
            val.append(v)
            if qlist[0] == '}':
                return ( val, qlist[1:])
            elif qlist[0] == ',':
                qlist = qlist[1:]
            else:
                break ;
        raise SyntaxError("list arguements not comma-separated")

    def Child_varspace( self, arglist, valuelist ):
        """Create a new (temporary VariableSpace structure based on a parent and the values applied to toi args
        used for macros and functions"""
        new_varspace = VariableSpace(self)
        for a,v in zip(arglist,valuelist):
            #print("zip",a,v)
            new_varspace.Add(a,v)
        #print("New Varspace, arglist=",arglist," vallist=",valuelist," result",new_varspace.local)
        return new_varspace

    def Function_apply( self, plist ):
        """ Called after a function found
        plist includes function as first element
        returns (val, qlist ) after closing paren
        """
        f = self.LookupFunction(plist[0])
        # function
        if len(plist)<3:
            raise SyntaxError("Bare function "+plist[0])
        if plist[1] != '(':
            raise SyntaxError("Function without arguments "+plist[0])
            
        (valuelist, qlist ) = self.Paren( plist[2:], len(f.args) )
        #print("fun_apply",val,qlist)
        
        # create a child varspace with these arguments bound
        new_varspace = self.Child_varspace( f.args, valuelist )
            
        return ( new_varspace.EvalP( f.expression ), qlist )
    
    def Macro_invokation_setup( self, plist ):
        """Called on a potential macro as first element of plist
        returns either:
        (Macro_instance,new_varspace)
        or
        (None,old_varspace)
        
        Note that it is assumed  that there in no code after the macro(args) expression
        """
        m = self.LookupMacro(plist[0])
        # macro
        if m:
            if len(plist)<3:
                raise SyntaxError("Bare macro "+plist[0])
            if plist[1] != '(':
                raise SyntaxError("Macro without arguments "+plist[0])
                
            (valuelist, qlist ) = self.Paren( plist[2:], len(m.args) )
            # create a child varspace with these arguments bound
            return ( m, self.Child_varspace( m.args, valuelist ) )
        else:
            return ( None, self )

    @staticmethod
    def Plist( line ):
        """Convert line to a separated list (by operator)"""
        return VariableSpace.linesplit.sub(r" \1 ",str(line)).split()   
    
    
    def Eval( self, plist ):
        """ Evaluate an expression """
        ( val, qlist ) = self.Compare( plist )
        return val
  
    def EvalP( self, line ):
        """ Evaluate an expression -- parse first """
        ( val, qlist ) = self.Compare( self.Plist(line) )
        return val
  

class Line:
    """Holds the data for a line of code
    includes address, text, etc
    and a list of lines of code"""

    parse_badlabel = re.compile(r"^[0-9$]")
    
    parse_comment =     re.compile(r"^(?P<noncomment>[^;]*);(?P<comment>.*)$")
    parse_label =     re.compile(r"^\s*(?P<label>\S+)\s*:\s*(?P<body>.*)$")

    parse_equ = re.compile(r"^\s*\S+\s*\.equ\b\s*\S.*$",flags=re.I)

    parse_function = re.compile(r"^\s*\.function\b",flags=re.I)

    parse_macro = re.compile(r"^\s*\.macro\b",flags=re.I)
    parse_endmacro = re.compile(r"^\s*\.endmacro\b",flags=re.I)

    parse_org = re.compile(r"^\s*\.org\b\s*(.*)$",flags=re.I)

    parse_if = re.compile(r"^\s*\.if\b\s*(.*)$",flags=re.I)
    parse_elif = re.compile(r"^\s*\.elif\b\s*(.*)$",flags=re.I)
    parse_else = re.compile(r"^\s*\.else\b",flags=re.I)
    parse_endif = re.compile(r"^\s*\.endif\b",flags=re.I)
    
    fullcode = [] # list
    org = 0x8000 # rom address of first statement
    varspace = VariableSpace()
    unresolved = set()
    macrodef = None # Class instance of current macro being read (1st pass, .MACRO and .ENDMACRO delimit)
    ifstate = If()
    ifdepth = 0
    
    program_counter = 0 # rom address of next statment
    
    def __init__( self, rawline, varspace=None ):
        """ start with a raw line of code """
        
        [label,self.body,comment] = self.Split( rawline )
        if Line.parse_badlabel.match(label):
            label = None
        
        #print("Line",rawline,self.label,self.body)
        self.address = Line.program_counter
        if varspace:
            self.varspace = varspace # can hide class
        else:
            self.varspace = Line.varspace
        
        if not Line.ifstate.State():
            # in non-active area.
            # just match if/endif
            # and look for next if state change
            if Line.parse_if.match(self.body):
                Line.ifdepth += 1
            elif Line.ifdepth > 0:
                # nested if's in inactive code -- only look for ENDIF 
                if Line.parse_endif.match(self.body):
                    Line.ifdepth -= 1
            elif Line.parse_elif.match(self.body):
                # Possibly change If state of current code (from False)
                Line.ifstate.ELIF( self.varspace.EvalP( Line.parse_elif.match(self.body).group(1) ) )
            elif Line.parse_else.match(self.body):
                # Possibly change If state of current code (from False)
                Line.ifstate.ELSE()
            elif Line.parse_endif.match(self.body):
                # Pop up an IF level
                Line.ifstate.ENDIF()
            
        elif Line.macrodef:
            # within a macro definition
            if Line.parse_endmacro.match(self.body):
                #leave macro
                Line.macrodef = None
            else:
                #add Line to macro list body
                self.varspace.Macro_add_line(Line.macrodef, rawline )

        else:
            # Not a macro, so try to process immediately or at least set up for pass 2
            
            if label:
                # Label -- resolvable since prograqm_counter is known
                self.varspace.Add(label,Line.program_counter)
                Line.unresolved.discard(label)

            if Line.parse_macro.match(self.body):
                #print("MACRO DEF",self.body)
                # macro definition
                Line.macrodef = Macro( self.body ) # flag in macro lines mode
                self.varspace.Macro_add( Line.macrodef )

            elif Line.parse_function.match(self.body):
                # function definition
                self.varspace.Function_add( Function( self.body ) )

            elif Line.parse_equ.match(self.body):
                # EQU item -- possibly unresolved now
                self.item = Equ( self.body )
                try:
                    self.varspace.Add( self.item.name, self.varspace.Eval( self.item.expression ) )
                    Line.unresolved.discard(self.item.name)
                except ResolutionError:
                    Line.fullcode.append(self)

            elif Line.parse_org.match(self.body):
                # Assign code start address ".ORG"
                try:
                    org = self.varspace.EvalP( Line.parse_org.match(self.body).group(1) )
                except ResolutionError:
                    print("Cannot evaluate program start location in: "+rawline)
                    raise
                self.varspace.Add( r".ORG", org )
                # an ORG line -- reset program counter
                Line.program_counter = org
                # redo the label address
                if label:
                    self.varspace.Add(label, Line.program_counter, overwrite=True)

            elif Line.parse_if.match(self.body):
                # If state in current context
                Line.ifstate.IF( self.varspace.EvalP( Line.parse_if.match(self.body).group(1) ) )
 
            elif Line.parse_elif.match(self.body):
                # If state in current context, aready true
                Line.ifstate.ELIF( False )
 
            elif Line.parse_else.match(self.body):
                # If state in current context
                Line.ifstate.ELSE()
 
            elif Line.parse_endif.match(self.body):
                # If state in current context
                Line.ifstate.ENDIF()
 
            elif self.dataitem():
                pass
                
            elif self.opitem():
                pass
 
            elif self.Macro_invokation():
                pass
 
            else:
                pass
        #print(len(self.varspace.local))
                 
    def RenderItem( self ):
        """ take an item and produce byte array (and any calculations) """
        #print(len(self.varspace.local))
        try:
            if isinstance(self.item,Data):
                #print("Data item at {:04X} is <{}>".format(self.address,self.body))
                return self.item.Render( self.varspace )
            elif isinstance(self.item,Op):
                return self.item.Render( self.varspace, self.address, self.body )
            else:
                raise ValueError("Unrecognized line type in second pass")
        except ResolutionError as var:
            print("Resolution Error where={:04X} who={} what={}".format(self.address,var,self.body))
            raise
        except:
            print("Other 2nd pass error. Address={:04X} Expression={}".format(self.address,self.body))
            raise
            
    @staticmethod
    def Render():
        """ Make second pass generating byte code """
        # First do EQU statments
        for it in Line.fullcode:
            if isinstance(it.item,Equ):
                try:
                    val = it.varspace.Eval( it.item.expression )
                except:
                    print("Evaluation error in assignment. Var={} Expression={}".format(it.item.name,it.item.expression))
                    raise
                it.varspace.Add( it.item.name, val )
                
        return bytearray(0).join([it.RenderItem() for it in Line.fullcode])
            
            
    def dataitem(self):
        """ If data item return True (and do all processing)"""
        #print("DATAITEM")
        try:
            self.item = Data( self.body )
            Line.program_counter += self.item.Length()
            Line.fullcode.append(self)
            return True
        except TypeError:
            return False
            
    def opitem(self):
        """ If data item return True (and do all processing)"""
        #print("OPITEM")
        self.item = Op.ParseOp( self.varspace, self.body )
        if self.item != None:
            Line.program_counter += self.item.Length()
            Line.fullcode.append(self)
            return True
        else:
            return False
            
    def Macro_invokation(self):
        """Try to apply a macro (will test if exists)"""
        plist = VariableSpace.Plist(self.body)
        if not plist:
            return False
        #print("MACRO INVOKE",self.body)
        (macro,new_varspace) = self.varspace.Macro_invokation_setup( plist )

        if macro:
            #print(new_varspace.local)
            for li in macro.expression:
                #print("Macro line",li)
                Line( li, new_varspace )
            return True
        return False
        
    @staticmethod
    def Split( line ):
        """Split into a list of [label,body,comment] with None if component missing"""
        d1 = Line.parse_comment.match(line)
        if d1:
            comment = d1.group('comment')
            li = d1.group('noncomment')
        else:
            comment = ''
            li = line

        d2 = Line.parse_label.match(li)        
        if d2:
            label = d2.group('label')
            body = d2.group('body')
        else:
            label=''
            body = li.strip()
        
        return [ label, body, comment]
    

def ReadAssemblerFile( f_asm ):
    """ Parse the assembly language file
    Motorola 6800 code """

    #first pass
    if f_asm:
        for line in f_asm.readlines():
            #print(filename,line)
            Line( line ) 

    # unresolved after first pass
    if len(Line.unresolved) > 0:
        print("Unresolved variables after first pass:",Line.unresolved)
        #print( VariableSpace.Functionspace )
    
    #secondpass to create assembly file
    binary = Line.Render()
    
    print("ROM length generated =",len(binary))
    return binary

def WriteAssemblerFile( rom_code, f_rom ):
    """ Write assembler output """
    if f_rom:
        f_rom.write(rom_code)

def LoadOplist():
    """Load all the 6800 assembly operators in Op class"""
    OpList = [              
        [ 1, "NOP", "", 1 , "IMPLIED", 2, "No Operation", ],
        [ 6, "TAP", "AF", 1 , "ACCUMULATOR", 2, "Transfer A to Status Flag Register", ],
        [ 7, "TPA", "A,Flags", 1 , "ACCUMULATOR", 2, "Transfer Status Flag Register to A", ],
        [ 8, "INX", "Index", 1 , "ACCUMULATOR", 4, "Increment the Index Register", ],
        [ 9, "DEX", "X", 1 , "ACCUMULATOR", 4, "Decrement the Index Register", ],
        [ 10, "CLV", "Flags", 1 , "ACCUMULATOR", 2, "Clear the Overflow Flag", ],
        [ 11, "SEV", "Flags", 1 , "ACCUMULATOR", 2, "Set the Overflow Flag", ],
        [ 12, "CLC", "Flags", 1 , "ACCUMULATOR", 2, "Clear the Carry Flag", ],
        [ 13, "SEC", "Flags", 1 , "ACCUMULATOR", 2, "Set the Carry Flag", ],
        [ 14, "CLI", "Flags", 1 , "ACCUMULATOR", 2, "Clear the Interrupt Flag", ],
        [ 15, "SEI", "Flags", 1 , "ACCUMULATOR", 2, "Set the Interrupt Flag", ],
        [ 16, "SBA", "AB", 1 , "ACCUMULATOR", 2, "Subtract B from A", ],
        [ 17, "CBA", "AB", 1 , "ACCUMULATOR", 2, "Compare B to A", ],
        [ 20, "!NBA!", "AB", 1 , "ACCUMULATOR", 2, "AND the ACCUMULATORs", ],
        [ 22, "TAB", "AB", 1 , "ACCUMULATOR", 2, "Transfer A to B", ],
        [ 23, "TBA", "AB", 1 , "ACCUMULATOR", 2, "Transfer B to A", ],
        [ 25, "DAA", "A", 1 , "ACCUMULATOR", 2, "Decimal Adjust A", ],
        [ 27, "ABA", "AB", 1 , "ACCUMULATOR", 2, "Add B to A", ],
        [ 32, "BRA", "", 2 , "RELATIVE", 4, "Branch Unconditional", ],
        [ 34, "BHI", "", 2 , "RELATIVE", 4, "Branch if ACCUM is Higher C&Z=0", ],
        [ 35, "BLS", "", 2 , "RELATIVE", 4, "Branch if ACCUM is Lower or Same C&Z=1", ],
        [ 36, "BCC", "", 2 , "RELATIVE", 4, "C=0", ],
        [ 37, "BCS", "", 2 , "RELATIVE", 4, "C=1", ],
        [ 38, "BNE", "", 2 , "RELATIVE", 4, "Z=0", ],
        [ 39, "BEQ", "", 2 , "RELATIVE", 4, "Z=1", ],
        [ 40, "BVC", "", 2 , "RELATIVE", 4, "V=0", ],
        [ 41, "BVS", "", 2 , "RELATIVE", 4, "V=1", ],
        [ 42, "BPL", "", 2 , "RELATIVE", 4, "N=0", ],
        [ 43, "BMI", "", 2 , "RELATIVE", 4, "N=1", ],
        [ 44, "BGE", "", 2 , "RELATIVE", 4, "Branch if 2's comp in A is >=, after SUB or Compare N=V", ],
        [ 45, "BLT", "", 2 , "RELATIVE", 4, "Branch if 2's comp in A is <, after SUB or Compare N<>V", ],
        [ 46, "BGT", "", 2 , "RELATIVE", 4, "Branch if 2's comp in A is >, after SUB or Compare Z=0 N=V", ],
        [ 47, "BLE", "", 2 , "RELATIVE", 4, "Branch if 2's comp in A is <=, after SUB or Compare Z=1 N<>V", ],
        [ 48, "TSX", "XS", 1 , "ACCUMULATOR", 4, "Transfer Stack Pointer to the Index Register", ],
        [ 49, "INS", "S", 1 , "ACCUMULATOR", 4, "Increment the Stack Pointer", ],
        [ 50, "PULA", "A", 1 , "ACCUMULATOR", 4, "Pull Data from Stack to A", ],
        [ 51, "PULB", "B", 1 , "ACCUMULATOR", 4, "Pull Data from Stack to B", ],
        [ 52, "DES", "S", 1 , "ACCUMULATOR", 4, "Decrement the Stack Pointer", ],
        [ 53, "TXS", "XS", 1 , "ACCUMULATOR", 4, "Transfer Index Register to the Stack Pointer", ],
        [ 54, "PSHA", "A", 1 , "ACCUMULATOR", 4, "Push A onto the Stack", ],
        [ 55, "PSHB", "B", 1 , "ACCUMULATOR", 4, "Push B onto the Stack", ],
        [ 57, "RTS", "", 1 , "IMPLIED", 5, "Return from Subroutine", ],
        [ 59, "RTI", "", 1 , "IMPLIED", 10, "Return from Interrupt", ],
        [ 62, "WAI", "", 1 , "IMPLIED", 9, "Wait for Interrupt", ],
        [ 63, "SWI", "", 1 , "IMPLIED", 12, "Software Interrupt", ],
        [ 64, "NEGA", "A", 1 , "ACCUMULATOR", 2, "Negate the A ACCUMULATOR", ],
        [ 67, "COMA", "A", 1 , "ACCUMULATOR", 2, "Complement the A ACCUMULATOR", ],
        [ 68, "LSRA", "A", 1 , "ACCUMULATOR", 2, "Logical Shift Right the A ACCUMULATOR 0->A->c", ],
        [ 70, "RORA", "A", 1 , "ACCUMULATOR", 2, "Rotate Right the A ACCUMULATOR c->A->c", ],
        [ 71, "ASRA", "A", 1 , "ACCUMULATOR", 2, "Arithmetic Shift Right the A ACCUMULATOR b7->A->c", ],
        [ 72, "ASLA", "A", 1 , "ACCUMULATOR", 2, "Arithmetic Shift Left the A ACCUMULATOR c<-A<-0", ],
        [ 73, "ROLA", "A", 1 , "ACCUMULATOR", 2, "Rotate Left the A ACCUMULATOR  c<-A<-c", ],
        [ 74, "DECA", "A", 1 , "ACCUMULATOR", 2, "Decrement the A ACCUMULATOR", ],
        [ 76, "INCA", "A", 1 , "ACCUMULATOR", 2, "Increment the A ACCUMULATOR", ],
        [ 77, "TSTA", "A", 1 , "ACCUMULATOR", 2, "Test the A ACCUMULATOR", ],
        [ 79, "CLRA", "A", 1 , "ACCUMULATOR", 2, "Clear the A ACCUMULATOR", ],
        [ 80, "NEGB", "B", 1 , "ACCUMULATOR", 2, "Negate the B ACCUMULATOR", ],
        [ 83, "COMB", "B", 1 , "ACCUMULATOR", 2, "Complement the B ACCUMULATOR", ],
        [ 84, "LSRB", "B", 1 , "ACCUMULATOR", 2, "Logical Shift Right the B ACCUMULATOR 0->B->c", ],
        [ 86, "RORB", "B", 1 , "ACCUMULATOR", 2, "Rotate Right the B ACCUMULATOR c->B->c", ],
        [ 87, "ASRB", "B", 1 , "ACCUMULATOR", 2, "Arithmetic Shift Right the B ACCUMULATOR b7->B->c", ],
        [ 88, "ASLB", "B", 1 , "ACCUMULATOR", 2, "Arithmetic Shift Left the B ACCUMULATOR c<-B<-0", ],
        [ 89, "ROLB", "B", 1 , "ACCUMULATOR", 2, "Rotate Left the B ACCUMULATOR c<-B<-c", ],
        [ 90, "DECB", "B", 1 , "ACCUMULATOR", 2, "Decrement the B ACCUMULATOR", ],
        [ 92, "INCB", "B", 1 , "ACCUMULATOR", 2, "Increment the B ACCUMULATOR", ],
        [ 93, "TSTB", "B", 1 , "ACCUMULATOR", 2, "Test the the B ACCUMULATOR", ],
        [ 95, "CLRB", "B", 1 , "ACCUMULATOR", 2, "Clear the B ACCUMULATOR", ],
        [ 96, "NEG", "MEM", 2 , "INDEXED", 7, "Negate the Memory Location", ],
        [ 99, "COM", "MEM", 2 , "INDEXED", 7, "Complement the Memory Location", ],
        [ 100, "LSR", "MEM", 2 , "INDEXED", 7, "Logical Shift Right 0->M->c", ],
        [ 102, "ROR", "MEM", 2 , "INDEXED", 7, "Rotate Right c->M->c", ],
        [ 103, "ASR", "MEM", 2 , "INDEXED", 7, "Arithmetic Shift Right b7->M->c", ],
        [ 104, "ASL", "MEM", 2 , "INDEXED", 7, "Arithmetic Shift Left c<-M<-0", ],
        [ 105, "ROL", "MEM", 2 , "INDEXED", 7, "Rotate Left c<-M<-c", ],
        [ 106, "DEC", "MEM", 2 , "INDEXED", 7, "Decrement the Memory Location", ],
        [ 108, "INC", "MEM", 2 , "INDEXED", 7, "Increment the Memory Location", ],
        [ 109, "TST", "MEM", 2 , "INDEXED", 7, "Test the Memory Location", ],
        [ 110, "JMP", "MEM", 2 , "INDEXED", 4, "Jump", ],
        [ 111, "CLR", "MEM", 2 , "INDEXED", 7, "Clear the Memory Location", ],
        [ 112, "NEG", "MEM", 3 , "EXTENDED", 6, "Negate the Memory Location", ],
        [ 115, "COM", "MEM", 3 , "EXTENDED", 6, "Complement the Memory Location", ],
        [ 116, "LSR", "MEM", 3 , "EXTENDED", 6, "Logical Shift Right Description 0->M->c", ],
        [ 118, "ROR", "MEM", 3 , "EXTENDED", 6, "Rotate Right  c->M->c", ],
        [ 119, "ASR", "MEM", 3 , "EXTENDED", 6, "Arithmetic Shift Right b7->M->c", ],
        [ 120, "ASL", "MEM", 3 , "EXTENDED", 6, "Arithmetic Shift Left c<-M<-0", ],
        [ 121, "ROL", "MEM", 3 , "EXTENDED", 6, "Rotate Left c<-M<-c", ],
        [ 122, "DEC", "MEM", 3 , "EXTENDED", 6, "Decrement the Memory Location", ],
        [ 124, "INC", "MEM", 3 , "EXTENDED", 6, "Increment the Memory Location", ],
        [ 125, "TST", "MEM", 3 , "EXTENDED", 6, "Test the Memory Location", ],
        [ 126, "JMP", "MEM", 3 , "EXTENDED", 3, "Jump", ],
        [ 127, "CLR", "MEM", 3 , "EXTENDED", 6, "Clear the Memory Location", ],
        [ 128, "SUBA", "A", 2 , "IMMEDIATE", 2, "Subtract Memory contents from the A ACCUMULATOR", ],
        [ 129, "CMPA", "A", 2 , "IMMEDIATE", 2, "Compare the contents of Memory to the A ACCUMULATOR", ],
        [ 130, "SBCA", "A", 2 , "IMMEDIATE", 2, "Subtract Mem and Carry Flag from the A ACCUMULATOR", ],
        [ 132, "ANDA", "A", 2 , "IMMEDIATE", 2, "AND the A ACCUMULATOR", ],
        [ 133, "BITA", "A", 2 , "IMMEDIATE", 2, "Bit Test the A ACCUMULATOR", ],
        [ 134, "LDAA", "A", 2 , "IMMEDIATE", 2, "Load the A ACCUMULATOR from Memory", ],
        [ 136, "EORA", "A", 2 , "IMMEDIATE", 2, "EXLCLUSIVE OR the A ACCUMULATOR", ],
        [ 137, "ADCA", "A", 2 , "IMMEDIATE", 2, "Add contents of Mem +Carry Flag to the A ACCUMULATOR", ],
        [ 138, "ORAA", "A", 2 , "IMMEDIATE", 2, "OR the A ACCUMULATOR", ],
        [ 139, "ADDA", "A", 2 , "IMMEDIATE", 2, "Add Memory contents to the A ACCUMULATOR", ],
        [ 140, "CPX", "X", 3 , "IMMEDIATE", 3, "Compare the contents of Mem to the Index Reg", ],
        [ 141, "BSR", "", 2 , "RELATIVE", 8, "Branch Subroutine", ],
        [ 142, "LDS", "S", 3 , "IMMEDIATE", 3, "Load the Stack Pointer", ],
        [ 144, "SUBA", "A", 2 , "DIRECT", 3, "Subtract Memory contents from the A ACCUMULATOR", ],
        [ 145, "CMPA", "A", 2 , "DIRECT", 3, "Compare the contents of Memory to the A ACCUMULATOR", ],
        [ 146, "SBCA", "A", 2 , "DIRECT", 3, "Subtract Mem and Carry Flag from the A ACCUMULATOR", ],
        [ 148, "ANDA", "A", 2 , "DIRECT", 3, "AND the A ACCUMULATOR", ],
        [ 149, "BITA", "A", 2 , "DIRECT", 3, "Bit Test the A ACCUMULATOR", ],
        [ 150, "LDAA", "A", 2 , "DIRECT", 3, "Load the A ACCUMULATOR from Memory", ],
        [ 151, "STAA", "A", 2 , "DIRECT", 4, "Store the A ACCUMULATOR in Memory", ],
        [ 152, "EORA", "A", 2 , "DIRECT", 3, "EXLCLUSIVE OR the A ACCUMULATOR", ],
        [ 153, "ADCA", "A", 2 , "DIRECT", 3, "Add contents of Mem +Carry Flag to the A ACCUMULATOR", ],
        [ 154, "ORAA", "A", 2 , "DIRECT", 3, "OR the A ACCUMULATOR", ],
        [ 155, "ADDA", "A", 2 , "DIRECT", 3, "Add Memory contents to the A ACCUMULATOR", ],
        [ 156, "CPX", "X", 2 , "DIRECT", 4, "Compare the contents of Mem to the Index Reg", ],
        [ 157, "!HCF!", "", 1 , "ACCUMULATOR", 100, "Halt and Catch Fire", ],
        [ 158, "LDS", "S", 2 , "DIRECT", 4, "Load the Stack Pointer", ],
        [ 159, "STS", "S", 2 , "DIRECT", 5, "Store the Stack Pointer", ],
        [ 160, "SUBA", "A", 2 , "INDEXED", 5, "Subtract Memory contents from the A ACCUMULATOR", ],
        [ 161, "CMPA", "A", 2 , "INDEXED", 5, "Compare the contents of Memory to the A ACCUMULATOR", ],
        [ 162, "SBCA", "A", 2 , "INDEXED", 5, "Subtract Mem and Carry Flag from the A ACCUMULATOR", ],
        [ 164, "ANDA", "A", 2 , "INDEXED", 5, "AND the A ACCUMULATOR", ],
        [ 165, "BITA", "A", 2 , "INDEXED", 5, "Bit Test the A ACCUMULATOR", ],
        [ 166, "LDAA", "A", 2 , "INDEXED", 5, "Load the A ACCUMULATOR from Memory", ],
        [ 167, "STAA", "A", 2 , "INDEXED", 6, "Store the A ACCUMULATOR in Memory", ],
        [ 168, "EORA", "A", 2 , "INDEXED", 5, "EXLCLUSIVE OR the A ACCUMULATOR", ],
        [ 169, "ADCA", "A", 2 , "INDEXED", 5, "Add contents of Mem +Carry Flag to the A ACCUMULATOR", ],
        [ 170, "ORAA", "A", 2 , "INDEXED", 5, "OR the A ACCUMULATOR", ],
        [ 171, "ADDA", "A", 2 , "INDEXED", 5, "Add Memory contents to the A ACCUMULATOR", ],
        [ 172, "CPX", "X", 2 , "INDEXED", 6, "Compare the contents of Mem to the Index Reg", ],
        [ 173, "JSR", "", 2 , "INDEXED", 8, "Jump to Subroutine", ],
        [ 174, "LDS", "S", 2 , "INDEXED", 6, "Load the Stack Pointer Description", ],
        [ 175, "STS", "S", 2 , "INDEXED", 7, "Store the Stack Pointer", ],
        [ 176, "SUBA", "A", 3 , "EXTENDED", 4, "Subtract Memory contents from the A ACCUMULATOR", ],
        [ 177, "CMPA", "A", 3 , "EXTENDED", 4, "Compare the contents of Memory to the A ACCUMULATOR", ],
        [ 178, "SBCA", "A", 3 , "EXTENDED", 4, "Subtract Mem and Carry Flag from the A ACCUMULATOR", ],
        [ 180, "ANDA", "A", 3 , "EXTENDED", 4, "AND the A ACCUMULATOR", ],
        [ 181, "BITA", "A", 3 , "EXTENDED", 4, "Bit Test the A ACCUMULATOR", ],
        [ 182, "LDAA", "A", 3 , "EXTENDED", 4, "Load the A ACCUMULATOR from Memory", ],
        [ 183, "STAA", "A", 3 , "EXTENDED", 5, "Store the A ACCUMULATOR in Memory", ],
        [ 184, "EORA", "A", 3 , "EXTENDED", 4, "EXLCLUSIVE OR the A ACCUMULATOR", ],
        [ 185, "ADCA", "A", 3 , "EXTENDED", 4, "Add contents of Mem +Carry Flag to the A ACCUMULATOR", ],
        [ 186, "ORAA", "A", 3 , "EXTENDED", 4, "OR the A ACCUMULATOR", ],
        [ 187, "ADDA", "A", 3 , "EXTENDED", 4, "Add Memory contents to the A ACCUMULATOR", ],
        [ 188, "CPX", "X", 3 , "EXTENDED", 5, "Compare the contents of Mem to the Index Reg", ],
        [ 189, "JSR", "", 3 , "EXTENDED", 9, "Jump to Subroutine", ],
        [ 190, "LDS", "S", 3 , "EXTENDED", 5, "Load the Stack Pointer", ],
        [ 191, "STS", "S", 3 , "EXTENDED", 6, "Store the Stack Pointer", ],
        [ 192, "SUBB", "B", 2 , "IMMEDIATE", 2, "Subtract Memory contents from the B ACCUMULATOR", ],
        [ 193, "CMPB", "B", 2 , "IMMEDIATE", 2, "Compare the contents of Memory to the B ACCUMULATOR", ],
        [ 194, "SBCB", "B", 2 , "IMMEDIATE", 2, "Subtract Mem and Carry Flag from the B ACCUMULATOR", ],
        [ 196, "ANDB", "B", 2 , "IMMEDIATE", 2, "AND the B ACCUMULATOR", ],
        [ 197, "BITB", "B", 2 , "IMMEDIATE", 2, "Bit Test the B ACCUMULATOR", ],
        [ 198, "LDAB", "B", 2 , "IMMEDIATE", 2, "Load the B ACCUMULATOR from Memory", ],
        [ 200, "EORB", "B", 2 , "IMMEDIATE", 2, "EXLCLUSIVE OR the B ACCUMULATOR", ],
        [ 201, "ADCB", "B", 2 , "IMMEDIATE", 2, "Add contents of Mem +Carry Flag to the B ACCUMULATOR", ],
        [ 202, "ORAB", "B", 2 , "IMMEDIATE", 2, "OR the B ACCUMULATOR", ],
        [ 203, "ADDB", "B", 2 , "IMMEDIATE", 2, "Add Memory contents to the B ACCUMULATOR", ],
        [ 206, "LDX", "X", 3 , "IMMEDIATE", 3, "Load the Index Register", ],
        [ 208, "SUBB", "B", 2 , "DIRECT", 3, "Subtract Memory contents from the B ACCUMULATOR", ],
        [ 209, "CMPB", "B", 2 , "DIRECT", 3, "Compare the contents of Memory to the B ACCUMULATOR", ],
        [ 210, "SBCB", "B", 2 , "DIRECT", 3, "Subtract Mem and Carry Flag from the B ACCUMULATOR", ],
        [ 212, "ANDB", "B", 2 , "DIRECT", 3, "AND the B ACCUMULATOR", ],
        [ 213, "BITB", "B", 2 , "DIRECT", 3, "Bit Test the B ACCUMULATOR", ],
        [ 214, "LDAB", "B", 2 , "DIRECT", 3, "Load the B ACCUMULATOR from Memory", ],
        [ 215, "STAB", "B", 2 , "DIRECT", 4, "Store the B ACCUMULATOR in Memory", ],
        [ 216, "EORB", "B", 2 , "DIRECT", 3, "EXLCLUSIVE OR the B ACCUMULATOR", ],
        [ 217, "ADCB", "B", 2 , "DIRECT", 3, "Add contents of Mem +Carry Flag to the B ACCUMULATOR", ],
        [ 218, "ORAB", "B", 2 , "DIRECT", 3, "OR the B ACCUMULATOR", ],
        [ 219, "ADDB", "B", 2 , "DIRECT", 3, "Add Memory contents to the B ACCUMULATOR", ],
        [ 221, "!HCF!", "", 1 , "IMPLIED", 100, "Halt and Catch Fire", ],
        [ 222, "LDX", "X", 2 , "DIRECT", 4, "Load the Index Register", ],
        [ 223, "STX", "X", 2 , "DIRECT", 5, "Store the Index Register", ],
        [ 224, "SUBB", "B", 2 , "INDEXED", 5, "Subtract Memory contents from the B ACCUMULATOR", ],
        [ 225, "CMPB", "B", 2 , "INDEXED", 5, "Compare the contents of Memory to the B ACCUMULATOR", ],
        [ 226, "SBCB", "B", 2 , "INDEXED", 5, "Subtract Mem and Carry Flag from the B ACCUMULATOR", ],
        [ 228, "ANDB", "B", 2 , "INDEXED", 5, "AND the B ACCUMULATOR", ],
        [ 229, "BITB", "B", 2 , "INDEXED", 5, "Bit Test the B ACCUMULATOR", ],
        [ 230, "LDAB", "B", 2 , "INDEXED", 5, "Load the B ACCUMULATOR from Memory", ],
        [ 231, "STAB", "B", 2 , "INDEXED", 6, "Store the B ACCUMULATOR in Memory", ],
        [ 232, "EORB", "B", 2 , "INDEXED", 5, "EXLCLUSIVE OR the B ACCUMULATOR", ],
        [ 233, "ADCB", "B", 2 , "INDEXED", 5, "Add contents of Mem +Carry Flag to the B ACCUMULATOR", ],
        [ 234, "ORAB", "B", 2 , "INDEXED", 5, "OR the B ACCUMULATOR", ],
        [ 235, "ADDB", "B", 2 , "INDEXED", 5, "Add Memory contents to the B ACCUMULATOR", ],
        [ 238, "LDX", "X", 2 , "INDEXED", 6, "Load the Index Register", ],
        [ 239, "STX", "X", 2 , "INDEXED", 7, "Store the Index Register", ],
        [ 240, "SUBB", "B", 3 , "EXTENDED", 4, "Subtract Memory contents from the B ACCUMULATOR", ],
        [ 241, "CMPB", "B", 3 , "EXTENDED", 4, "Compare the contents of Memory to the B ACCUMULATOR", ],
        [ 242, "SBCB", "B", 3 , "EXTENDED", 4, "Subtract Mem and Carry Flag from the B ACCUMULATOR", ],
        [ 244, "ANDB", "B", 3 , "EXTENDED", 4, "AND the B ACCUMULATOR", ],
        [ 245, "BITB", "B", 3 , "EXTENDED", 4, "Bit Test the B ACCUMULATOR", ],
        [ 246, "LDAB", "B", 3 , "EXTENDED", 4, "Load the B ACCUMULATOR from Memory", ],
        [ 247, "STAB", "B", 3 , "EXTENDED", 5, "Store the B ACCUMULATOR in Memory", ],
        [ 248, "EORB", "B", 3 , "EXTENDED", 4, "EXLCLUSIVE OR the B ACCUMULATOR", ],
        [ 249, "ADCB", "B", 3 , "EXTENDED", 4, "Add contents of Mem +Carry Flag to the B ACCUMULATOR", ],
        [ 250, "ORAB", "B", 3 , "EXTENDED", 4, "OR the B ACCUMULATOR", ],
        [ 251, "ADDB", "B", 3 , "EXTENDED", 4, "Add Memory contents to the B ACCUMULATOR", ],
        [ 254, "LDX", "X", 3 , "EXTENDED", 5, "Load the Index Register", ],
        [ 255, "STX", "X", 3 , "EXTENDED", 6, "Store the Index Register", ],
    ]
    for o in OpList:
        Op(o)
 
class OldROM:
    """ compare generated machine code to another machine code"""
    def __init__( self, filename="REV5370B.ROM" ):
        with open( filename, "rb" ) as rom:
            self.data = rom.read()
        print( "ROM length original = ",len(self.data))

    def compare( self, new_rom ):
        if new_rom == self.data :
            print("Assembled code matches!!!")
        else:
            print("Assembled code does not match")
            self.PrintBinaryDiff(new_rom, Line.varspace.Lookup( '.ORG' ) )
 
    def PrintBinaryDiff( self, b2, offset ):
        """Print the difference in the two files with 10 byte context"""
        b1 = self.data
        lb1 = len(b1)
        lb2 = len(b2)

        if lb1 < lb2:
            for i in range( lb1 ):
                if b1[i] != b2[i]:
                    loc = i
                    break ;
            else:
                loc = lb1 + 1
        elif lb2 < lb2:
            for i in range( lb2 ):
                if b1[i] != b2[i]:
                    loc = i
                    break ;
            else:
                loc = lb2 + 1
        else:
            for i in range( lb1 ):
                if b1[i] != b2[i]:
                    loc = i
                    break ;
            else:
                print("Identical strings")
                return
            
        contextR = 10
        contextL = 5
        print("String lengths {:04X} and {:04X}".format(lb1,lb2))
        print("Difference at {:04X}".format(loc+offset))
        up1 = min(loc+contextR,lb1)
        up2 = min(loc+contextR,lb2)
        lo = max(0,loc-contextL)
        print("{:04X}:".format(lo+offset))

        if lo < lb1:
            s = ""
            for b in range(lo,up1):
                if b == loc:
                    s += "<{:02X}> ".format(b1[b]) 
                else:
                    s += "{:02X} ".format(b1[b]) 
            print(s)

        if lo < lb2:
            s = ""
            for b in range(lo,up2):
                if b == loc:
                    s += "<{:02X}> ".format(b2[b]) 
                else:
                    s += "{:02X} ".format(b2[b]) 
            print(s)

def UnusedListing( f_uu ):
    """ List the unused symbols """
    vs = Line.varspace
    f_uu.write("Unused symbols:\n")
    for u in sorted(Line.varspace.unused):
        val = vs.EvalP(vs.local[u])
        if val > 255:
            f_uu.write( "{:26}= {:04X}\n".format(u,val) )
        else:
            f_uu.write( "{:26}=   {:02X}\n".format(u,val) )
        
def VariableListing( f_list ):
    """ List the global symbol list and values """
    vs = Line.varspace
    for k , v in sorted(vs.local.items()):
        val = vs.EvalP(v)
        if val > 255:
            f_list.write( "{:26}= {:04X}\n".format(k,val) )
        else:
            f_list.write( "{:26}=   {:02X}\n".format(k,val) )


def CommandLine():
    """Setup argparser object to process the command line"""
    cl = argparse.ArgumentParser(description="Motorola 6800 microprocessor assembly language compiler")
    cl.add_argument("-r","--ROM",help="Existing ROM file to compare with compiled code",nargs='?',default='REV5370B.ROM')
    cl.add_argument("-p","--PROG",help="Assembly language code to compile",nargs='?', type=argparse.FileType('r'),default=sys.stdin)
    cl.add_argument("-o","--OUTPUT",help="Assembly output (ROM file)",nargs='?', type=argparse.FileType('wb'))
    cl.add_argument("-l","--LIST",help="List all defined symbols and their values",nargs='?',type=argparse.FileType('w'), const=sys.stdout, default=False)
    cl.add_argument("-u","--UNUSED",help="List all unused symbols and their values",nargs='?',type=argparse.FileType('w'), const=sys.stdout, default=False)
    cl.add_argument("-i","--INSTRUCTION",help="Show M6800 Instgruction set sorted by code|name|mode|flags|size|time|comment",nargs='?',default=None, const='code')
    return cl.parse_args()

    """
    >>> parser.add_argument('infile', nargs='?', type=argparse.FileType('r'),
    ...                     default=sys.stdin)
    >>> parser.add_argument('outfile', nargs='?', type=argparse.FileType('w'),
    ...
                         default=sys.stdout)
                         """
    
def main(args):
    
    # Load assembly language
    LoadOplist()
    
    args = CommandLine() # Get args from command line
    
    if args.INSTRUCTION:
        Op.Show( args.INSTRUCTION )
        return 0
    
    rom_assembled = ReadAssemblerFile(args.PROG)

    OldROM(args.ROM).compare(rom_assembled)
    
    if args.OUTPUT:
        WriteAssemblerFile( rom_assembled, args.OUTPUT )
        
    if args.UNUSED :
        UnusedListing( args.UNUSED )
        
    if args.LIST :
        VariableListing( args.LIST )
    
    return 0

if __name__ == '__main__':
    import sys # for system-related
    import re # for regular expressions
    import argparse # for parsing the command line
    
    sys.exit(main(sys.argv))
