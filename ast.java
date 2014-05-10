import java.io.*;
import java.util.*;

// **********************************************************************
// The ASTnode class defines the nodes of the abstract-syntax tree that
// represents a Mini program.
//
// Internal nodes of the tree contain pointers to children, organized
// either in a list (for nodes that may have a variable number of 
// children) or as a fixed set of fields.
//
// The nodes for literals and ids contain line and character number
// information; for string literals and identifiers, they also contain a
// string; for integer literals, they also contain an integer value.
//
// Here are all the different kinds of AST nodes and what kinds of children
// they have.  All of these kinds of AST nodes are subclasses of "ASTnode".
// Indentation indicates further subclassing:
//
//     Subclass            Kids
//     --------            ----
//     ProgramNode         DeclListNode
//     DeclListNode        linked list of DeclNode
//     DeclNode:
//       VarDeclNode       TypeNode, IdNode, int
//       FnDeclNode        TypeNode, IdNode, FormalsListNode, FnBodyNode
//       FormalDeclNode    TypeNode, IdNode
//       StructDeclNode    IdNode, DeclListNode
//
//     FormalsListNode     linked list of FormalDeclNode
//     FnBodyNode          DeclListNode, StmtListNode
//     StmtListNode        linked list of StmtNode
//     ExpListNode         linked list of ExpNode
//
//     TypeNode:
//       IntNode           -- none --
//       BoolNode          -- none --
//       VoidNode          -- none --
//       StructNode        IdNode
//
//     StmtNode:
//       AssignStmtNode      AssignNode
//       PostIncStmtNode     ExpNode
//       PostDecStmtNode     ExpNode
//       ReadStmtNode        ExpNode
//       WriteStmtNode       ExpNode
//       IfStmtNode          ExpNode, DeclListNode, StmtListNode
//       IfElseStmtNode      ExpNode, DeclListNode, StmtListNode,
//                                    DeclListNode, StmtListNode
//       WhileStmtNode       ExpNode, DeclListNode, StmtListNode
//       CallStmtNode        CallExpNode
//       ReturnStmtNode      ExpNode
//
//     ExpNode:
//       IntLitNode          -- none --
//       StrLitNode          -- none --
//       TrueNode            -- none --
//       FalseNode           -- none --
//       IdNode              -- none --
//       DotAccessNode       ExpNode, IdNode
//       AssignNode          ExpNode, ExpNode
//       CallExpNode         IdNode, ExpListNode
//       UnaryExpNode        ExpNode
//         UnaryMinusNode
//         NotNode
//       BinaryExpNode       ExpNode ExpNode
//         PlusNode     
//         MinusNode
//         TimesNode
//         DivideNode
//         AndNode
//         OrNode
//         EqualsNode
//         NotEqualsNode
//         LessNode
//         GreaterNode
//         LessEqNode
//         GreaterEqNode
//
// Here are the different kinds of AST nodes again, organized according to
// whether they are leaves, internal nodes with linked lists of kids, or
// internal nodes with a fixed number of kids:
//
// (1) Leaf nodes:
//        IntNode,   BoolNode,  VoidNode,  IntLitNode,  StrLitNode,
//        TrueNode,  FalseNode, IdNode
//
// (2) Internal nodes with (possibly empty) linked lists of children:
//        DeclListNode, FormalsListNode, StmtListNode, ExpListNode
//
// (3) Internal nodes with fixed numbers of kids:
//        ProgramNode,     VarDeclNode,     FnDeclNode,     FormalDeclNode,
//        StructDeclNode,  FnBodyNode,      StructNode,     AssignStmtNode,
//        PostIncStmtNode, PostDecStmtNode, ReadStmtNode,   WriteStmtNode   
//        IfStmtNode,      IfElseStmtNode,  WhileStmtNode,  CallStmtNode
//        ReturnStmtNode,  DotAccessNode,   AssignExpNode,  CallExpNode,
//        UnaryExpNode,    BinaryExpNode,   UnaryMinusNode, NotNode,
//        PlusNode,        MinusNode,       TimesNode,      DivideNode,
//        AndNode,         OrNode,          EqualsNode,     NotEqualsNode,
//        LessNode,        GreaterNode,     LessEqNode,     GreaterEqNode
//
// **********************************************************************

// **********************************************************************
// ASTnode class (base class for all other kinds of nodes)
// **********************************************************************

abstract class ASTnode { 
    // every subclass must provide an unparse operation
    abstract public void unparse(PrintWriter p, int indent);

    // this method can be used by the unparse methods to do indenting
    protected void doIndent(PrintWriter p, int indent) {
        for (int k=0; k<indent; k++) p.print(" ");
    }
}

// **********************************************************************
// ProgramNode,  DeclListNode, FormalsListNode, FnBodyNode,
// StmtListNode, ExpListNode
// **********************************************************************

class ProgramNode extends ASTnode {
    public ProgramNode(DeclListNode L) {
        myDeclList = L;
    }
    
    /**
     * Generates MIPS Code for node
     */
    public void codegen() {
      // Call myDeclList's Code Gen
      System.out.println("Program Node's codegen called");
      myDeclList.codegen();
    }


    /**
     * nameAnalysis
     * Creates an empty symbol table for the outermost scope, then processes
     * all of the globals, struct defintions, and functions in the program.
     */
    public void nameAnalysis() {
        SymTable symTab = new SymTable();
        myDeclList.nameAnalysis(symTab);
    }
    
    /**
     * typeCheck
     */
    public void typeCheck() {
        myDeclList.typeCheck();
    }
    
    public void unparse(PrintWriter p, int indent) {
        myDeclList.unparse(p, indent);
    }

    // 1 kid
    private DeclListNode myDeclList;
}

class DeclListNode extends ASTnode {
    //NO CODEGEN for DeclListNode within FnBodyNode
    public DeclListNode(List<DeclNode> S) {
        myDecls = S;
    }
    
    /**
     * Generates MIPS Code for node
     */
    public void codegen() {
      System.out.println("DeclListNode's codegen called");
      for (DeclNode n : myDecls)
         n.codegen();
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, process all of the decls in the list.
     */
    public void nameAnalysis(SymTable symTab) {
        nameAnalysis(symTab, symTab);
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab and a global symbol table globalTab
     * (for processing struct names in variable decls), process all of the 
     * decls in the list.
     */    
    public void nameAnalysis(SymTable symTab, SymTable globalTab) {
        for (DeclNode node : myDecls) {
            if (node instanceof VarDeclNode) {
                ((VarDeclNode)node).nameAnalysis(symTab, globalTab);
            } else {
                node.nameAnalysis(symTab);
            }
        }
    }    
    
    /**
     * typeCheck
     */
    public void typeCheck() {
        boolean hasMain = false;
        for (DeclNode node : myDecls) {
            if(node instanceof FnDeclNode){
                String functionName = ((FnDeclNode)node).fnName();
                if(functionName.equals("main")){
                    hasMain = true;
                }
            }
            node.typeCheck();
        }
        if(!hasMain){
            ErrMsg.fatal(0, 0, "No main function");
        }
    }
    
    public void unparse(PrintWriter p, int indent) {
        Iterator it = myDecls.iterator();
        try {
            while (it.hasNext()) {
                ((DeclNode)it.next()).unparse(p, indent);
            }
        } catch (NoSuchElementException ex) {
            System.err.println("unexpected NoSuchElementException in DeclListNode.print");
            System.exit(-1);
        }
    }
    public int getDeclListSize(){
        if(myDecls != null){
           return myDecls.size(); 
        }
        return 0; 
    }
    // list of kids (DeclNodes)
    private List<DeclNode> myDecls;
}

class FormalsListNode extends ASTnode {
    //NO CODEGEN
    public FormalsListNode(List<FormalDeclNode> S) {
        myFormals = S;
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * for each formal decl in the list
     *     process the formal decl
     *     if there was no error, add type of formal decl to list
     */
    public List<Type> nameAnalysis(SymTable symTab) {
        List<Type> typeList = new LinkedList<Type>();
        for (FormalDeclNode node : myFormals) {
            Sym sym = node.nameAnalysis(symTab);
            if (sym != null) {
                typeList.add(sym.getType());
            }
        }
        return typeList;
    }    
    
    /**
     * Return the number of formals in this list.
     */
    public int length() {
        return myFormals.size();
    }
    
    public void unparse(PrintWriter p, int indent) {
        Iterator<FormalDeclNode> it = myFormals.iterator();
        if (it.hasNext()) { // if there is at least one element
            it.next().unparse(p, indent);
            while (it.hasNext()) {  // print the rest of the list
                p.print(", ");
                it.next().unparse(p, indent);
            }
        } 
    }

    // list of kids (FormalDeclNodes)
    private List<FormalDeclNode> myFormals;
}

class FnBodyNode extends ASTnode {
    public FnBodyNode(DeclListNode declList, StmtListNode stmtList) {
        myDeclList = declList;
        myStmtList = stmtList;
    }
    /**
     * Generate MIPS code for this node
     */
    public void codegen(String eLbl){
      myStmtList.codegen(eLbl);
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * - process the declaration list
     * - process the statement list
     */
    public void nameAnalysis(SymTable symTab) {
        myDeclList.nameAnalysis(symTab);
        myStmtList.nameAnalysis(symTab);
    } 

    public int numDecls(){
        return myDeclList.getDeclListSize();
    }   
 
    /**
     * typeCheck
     */
    public void typeCheck(Type retType) {
        myStmtList.typeCheck(retType);
    }    
          
    public void unparse(PrintWriter p, int indent) {
        myDeclList.unparse(p, indent);
        myStmtList.unparse(p, indent);
    }

    // 2 kids
    private DeclListNode myDeclList;
    private StmtListNode myStmtList;
}

class StmtListNode extends ASTnode {
    public StmtListNode(List<StmtNode> S) {
        myStmts = S;
    }
    
    /**
     * Generate MIPS code for this node
     */
    public void codegen(String eLbl) {
      System.out.println("StmtListNode's codegen called");
      for (StmtNode n : myStmts)
         n.codegen(eLbl);
    }
    /**
     * nameAnalysis
     * Given a symbol table symTab, process each statement in the list.
     */
    public void nameAnalysis(SymTable symTab) {
        for (StmtNode node : myStmts) {
            node.nameAnalysis(symTab);
        }
    }    
    
    /**
     * typeCheck
     */
    public void typeCheck(Type retType) {
        for(StmtNode node : myStmts) {
            node.typeCheck(retType);
        }
    }
    
    public void unparse(PrintWriter p, int indent) {
        Iterator<StmtNode> it = myStmts.iterator();
        while (it.hasNext()) {
            it.next().unparse(p, indent);
        }
    }

    // list of kids (StmtNodes)
    private List<StmtNode> myStmts;
}

class ExpListNode extends ASTnode {
    public ExpListNode(List<ExpNode> S) {
        myExps = S;
    }
    
    /**
     * Generate MIPS code for this node
     */
    public void codegen() {
      // Push evaluations of all exps in myExps onto stack
      for (ExpNode n : myExps)
         n.codegen();
    }
    public int size() {
        return myExps.size();
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, process each exp in the list.
     */
    public void nameAnalysis(SymTable symTab) {
        for (ExpNode node : myExps) {
            node.nameAnalysis(symTab);
        }
    }
    
    /**
     * typeCheck
     */
    public void typeCheck(List<Type> typeList) {
        int k = 0;
        try {
            for (ExpNode node : myExps) {
                Type actualType = node.typeCheck();     // actual type of arg
                
                if (!actualType.isErrorType()) {        // if this is not an error
                    Type formalType = typeList.get(k);  // get the formal type
                    if (!formalType.equals(actualType)) {
                        ErrMsg.fatal(node.lineNum(), node.charNum(),
                                     "Type of actual does not match type of formal");
                    }
                }
                k++;
            }
        } catch (NoSuchElementException e) {
            System.err.println("unexpected NoSuchElementException in ExpListNode.typeCheck");
            System.exit(-1);
        }
    }
    
    public void unparse(PrintWriter p, int indent) {
        Iterator<ExpNode> it = myExps.iterator();
        if (it.hasNext()) { // if there is at least one element
            it.next().unparse(p, indent);
            while (it.hasNext()) {  // print the rest of the list
                p.print(", ");
                it.next().unparse(p, indent);
            }
        } 
    }

    // list of kids (ExpNodes)
    private List<ExpNode> myExps;
}

// **********************************************************************
// DeclNode and its subclasses
// **********************************************************************

abstract class DeclNode extends ASTnode {
    /**
     * Note: a formal decl needs to return a sym
     */
    abstract public Sym nameAnalysis(SymTable symTab);

    // default version of typeCheck for non-function decls
    public void typeCheck() { }
    // default version of codegen
    public void codegen() { }
}

class VarDeclNode extends DeclNode {
    //type name
    public VarDeclNode(TypeNode type, IdNode id, int size) {
        myType = type;
        myId = id;
        mySize = size;
    }

    /**
     * nameAnalysis (overloaded)
     * Given a symbol table symTab, do:
     * if this name is declared void, then error
     * else if the declaration is of a struct type, 
     *     lookup type name (globally)
     *     if type name doesn't exist, then error
     * if no errors so far,
     *     if name has already been declared in this scope, then error
     *     else add name to local symbol table     
     *
     * symTab is local symbol table (say, for struct field decls)
     * globalTab is global symbol table (for struct type names)
     * symTab and globalTab can be the same
     */
    public Sym nameAnalysis(SymTable symTab) {
        return nameAnalysis(symTab, symTab);
    }
    
    public Sym nameAnalysis(SymTable symTab, SymTable globalTab) {
        boolean badDecl = false;
        String name = myId.name();
        Sym sym = null;
        IdNode structId = null;

        if (myType instanceof VoidNode) {  // check for void type
            ErrMsg.fatal(myId.lineNum(), myId.charNum(), 
                         "Non-function declared void");
            badDecl = true;        
        }
        
        else if (myType instanceof StructNode) {
            structId = ((StructNode)myType).idNode();
            sym = globalTab.lookupGlobal(structId.name());
            
            // if the name for the struct type is not found, 
            // or is not a struct type
            if (sym == null || !(sym instanceof StructDefSym)) {
                ErrMsg.fatal(structId.lineNum(), structId.charNum(), 
                             "Invalid name of struct type");
                badDecl = true;
            }
            else {
                structId.link(sym);
            }
        }
        
        if (symTab.lookupLocal(name) != null) {
            ErrMsg.fatal(myId.lineNum(), myId.charNum(), 
                         "Multiply declared identifier");
            badDecl = true;            
        }
        
        if (!badDecl) {  // insert into symbol table
            try {
                if (myType instanceof StructNode) {
                    sym = new StructSym(structId);
                }
                else {
                    sym = new Sym(myType.type());
                }
                symTab.addDecl(name, sym);
                myId.link(sym);
            } catch (DuplicateSymException ex) {
                System.err.println("Unexpected DuplicateSymException " +
                                   " in VarDeclNode.nameAnalysis");
                System.exit(-1);
            } catch (EmptySymTableException ex) {
                System.err.println("Unexpected EmptySymTableException " +
                                   " in VarDeclNode.nameAnalysis");
                System.exit(-1);
            }
        }
        
        return sym;
    }
    /**
     * Generate MIPS code for this node
     */
    // Make sure this only gets called for glbl vars!
    public void codegen(){
        myId.sym().setGlbl();
        Codegen.generate(" .data");
        Codegen.generate(" .align 2");
        Codegen.generateLabeled("_" + myId.name(), ".space "+myType.size(),"");
    }
    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        myType.unparse(p, 0);
        p.print(" ");
        p.print(myId.name());
        p.println(";");
    }

    // 3 kids
    private TypeNode myType;
    private IdNode myId;
    private int mySize;  // use value NOT_STRUCT if this is not a struct type

    public static int NOT_STRUCT = -1;
}

class FnDeclNode extends DeclNode {
    //NO CODEGEN for type, ID, formalslist 
    public FnDeclNode(TypeNode type,
                      IdNode id,
                      FormalsListNode formalList,
                      FnBodyNode body) {
        myType = type;
        myId = id;
        myFormalsList = formalList;
        myBody = body;
    }
    
    public void codegen(){
        // Generate special code for main function, get param & local sizes
        boolean isMain = myId.name().equals("main");
        int sizeParams = ((FnSym)(myId.sym())).getParamsSize();
        int sizeLocals = ((FnSym)(myId.sym())).getLocalsSize();
        // Exit label, may want to change later to ensure uniqueness
        String eLbl = "_" + myId.name() + "_Exit";
        ((FnSym)myId.sym()).setExit(eLbl);
        
        Codegen.generate(".text");
        //Preamble
        if (isMain) {
         Codegen.generate(".globl main");
         ((FnSym)myId.sym()).setEntry(myId.name());
         Codegen.genLabel(myId.name(), "METHOD ENTRY");
         Codegen.genLabel("__start", "add __start label for main only");
        } else {
         ((FnSym)myId.sym()).setEntry("_" + myId.name());
         Codegen.genLabel("_" + myId.name(), "METHOD ENTRY");
        }

        //Prologue
        Codegen.genPush("$ra"); //push ret addr
        Codegen.genPush("$fp"); //push ctrl link
        //set new FP
        Codegen.generate("addu", "$fp", "$sp", 8+sizeParams);
        //push space for local vars, if we have them... 
        if (sizeLocals != 0)
         Codegen.generateIndexed("lw", "$sp", "$sp", -sizeLocals);
        
        //Body
        myBody.codegen(eLbl);
        
        //Epilogue
        // generate fn exit label
        Codegen.genLabel(eLbl, "FUNCTION EXIT");
        //load ret addr
        Codegen.generateIndexed("lw", "$ra", "$fp", -sizeParams);
        //save curr FP
        Codegen.generate("move","$t0","$fp");
        //restore old FP
        Codegen.generateIndexed("lw", "$fp", "$fp", -sizeParams-4);
        //restore old SP
        Codegen.generate("move", "$sp", "$t0");
        
        //return
        if (isMain) {
         // Exit program
         Codegen.generate("li","$v0",10);
         Codegen.generate("syscall");
        } else {
         // Exit function
         Codegen.generate("jr", "$ra");
        }
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * if this name has already been declared in this scope, then error
     * else add name to local symbol table
     * in any case, do the following:
     *     enter new scope
     *     process the formals
     *     if this function is not multiply declared,
     *         update symbol table entry with types of formals
     *     process the body of the function
     *     exit scope
     */
    public Sym nameAnalysis(SymTable symTab) {
        String name = myId.name();
        FnSym sym = null;
        if (symTab.lookupLocal(name) != null) {
            ErrMsg.fatal(myId.lineNum(), myId.charNum(),
                         "Multiply declared identifier");
        }
        
        else { // add function name to local symbol table
            try {
                //when declaring FnSym, formals size is set in constructor
                sym = new FnSym(myType.type(), myFormalsList.length());
                symTab.addDecl(name, sym);
                myId.link(sym);
            } catch (DuplicateSymException ex) {
                System.err.println("Unexpected DuplicateSymException " +
                                   " in FnDeclNode.nameAnalysis");
                System.exit(-1);
            } catch (EmptySymTableException ex) {
                System.err.println("Unexpected EmptySymTableException " +
                                   " in FnDeclNode.nameAnalysis");
                System.exit(-1);
            }
        }
        
        symTab.addScope();  // add a new scope for locals and params
        
        // process the formals
        List<Type> typeList = myFormalsList.nameAnalysis(symTab);
        if (sym != null) {
            sym.addFormals(typeList);
        }
        
        myBody.nameAnalysis(symTab); // process the function body
        //set locals size
        sym.setLocalsSize((myBody.numDecls())*4);
        
        try {
            symTab.removeScope();  // exit scope
        } catch (EmptySymTableException ex) {
            System.err.println("Unexpected EmptySymTableException " +
                               " in FnDeclNode.nameAnalysis");
            System.exit(-1);
        }
        
        return null;
    } 
       
    /**
     * typeCheck
     */
    public void typeCheck() {
        myBody.typeCheck(myType.type());
    }
        
    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        myType.unparse(p, 0);
        p.print(" ");
        p.print(myId.name());
        p.print("(");
        myFormalsList.unparse(p, 0);
        p.println(") {");
        myBody.unparse(p, indent+4);
        p.println("}\n");
    }
    public String fnName(){
        return myId.name();
    }
    // 4 kids
    private TypeNode myType;
    private IdNode myId;
    private FormalsListNode myFormalsList;
    private FnBodyNode myBody;
}

class FormalDeclNode extends DeclNode {
    public FormalDeclNode(TypeNode type, IdNode id) {
        myType = type;
        myId = id;
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * if this formal is declared void, then error
     * else if this formal is already in the local symble table,
     *     then issue multiply declared error message and return null
     * else add a new entry to the symbol table and return that Sym
     */
    public Sym nameAnalysis(SymTable symTab) {
        String name = myId.name();
        boolean badDecl = false;
        Sym sym = null;
        
        if (myType instanceof VoidNode) {
            ErrMsg.fatal(myId.lineNum(), myId.charNum(), 
                         "Non-function declared void");
            badDecl = true;        
        }
        
        if (symTab.lookupLocal(name) != null) {
            ErrMsg.fatal(myId.lineNum(), myId.charNum(), 
                         "Multiply declared identifier");
            badDecl = true;
        }
        
        if (!badDecl) {  // insert into symbol table
            try {
                sym = new Sym(myType.type());
                symTab.addDecl(name, sym);
                myId.link(sym);
            } catch (DuplicateSymException ex) {
                System.err.println("Unexpected DuplicateSymException " +
                                   " in VarDeclNode.nameAnalysis");
                System.exit(-1);
            } catch (EmptySymTableException ex) {
                System.err.println("Unexpected EmptySymTableException " +
                                   " in VarDeclNode.nameAnalysis");
                System.exit(-1);
            }
        }
        
        return sym;
    }    
    
    public void unparse(PrintWriter p, int indent) {
        myType.unparse(p, 0);
        p.print(" ");
        p.print(myId.name());
    }

    // 2 kids
    private TypeNode myType;
    private IdNode myId;
}

class StructDeclNode extends DeclNode {
    //NO CODEGEN
    public StructDeclNode(IdNode id, DeclListNode declList) {
        myId = id;
        myDeclList = declList;
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * if this name is already in the symbol table,
     *     then multiply declared error (don't add to symbol table)
     * create a new symbol table for this struct definition
     * process the decl list
     * if no errors
     *     add a new entry to symbol table for this struct
     */
    public Sym nameAnalysis(SymTable symTab) {
        String name = myId.name();
        boolean badDecl = false;
        
        if (symTab.lookupLocal(name) != null) {
            ErrMsg.fatal(myId.lineNum(), myId.charNum(), 
                         "Multiply declared identifier");
            badDecl = true;            
        }

        SymTable structSymTab = new SymTable();
        
        // process the fields of the struct
        myDeclList.nameAnalysis(structSymTab, symTab);
        
        if (!badDecl) {
            try {   // add entry to symbol table
                StructDefSym sym = new StructDefSym(structSymTab);
                symTab.addDecl(name, sym);
                myId.link(sym);
            } catch (DuplicateSymException ex) {
                System.err.println("Unexpected DuplicateSymException " +
                                   " in StructDeclNode.nameAnalysis");
                System.exit(-1);
            } catch (EmptySymTableException ex) {
                System.err.println("Unexpected EmptySymTableException " +
                                   " in StructDeclNode.nameAnalysis");
                System.exit(-1);
            }
        }
        
        return null;
    }    
    
    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        p.print("struct ");
        p.print(myId.name());
        p.println("{");
        myDeclList.unparse(p, indent+4);
        doIndent(p, indent);
        p.println("};\n");

    }

    // 2 kids
    private IdNode myId;
    private DeclListNode myDeclList;
}

// **********************************************************************
// TypeNode and its Subclasses
// **********************************************************************

abstract class TypeNode extends ASTnode {
    /* all subclasses must provide a type method */
    abstract public Type type();
    public int size() { return 4;}
}

class IntNode extends TypeNode {
    public IntNode() {
    }
    
     /**
     * type
     */
    public Type type() {
        return new IntType();
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("int");
    }
}

class BoolNode extends TypeNode {
    public BoolNode() {
    }

    /**
     * type
     */
    public Type type() {
        return new BoolType();
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("bool");
    }
}

class VoidNode extends TypeNode {
    public VoidNode() {
    }
    
    /**
     * type
     */
    public Type type() {
        return new VoidType();
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("void");
    }
}

class StructNode extends TypeNode {
    public StructNode(IdNode id) {
        myId = id;
    }

    public IdNode idNode() {
        return myId;
    }
    
    /**
     * type
     */
    public Type type() {
        return new StructType(myId);
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("struct ");
        p.print(myId.name());
    }
    
    // 1 kid
    private IdNode myId;
}

// **********************************************************************
// StmtNode and its subclasses
// **********************************************************************

abstract class StmtNode extends ASTnode {
    abstract public void nameAnalysis(SymTable symTab);
    abstract public void typeCheck(Type retType);
    // default codegen for StmtNode
    public void codegen(String eLbl) {
      System.out.println("StmtNode's codegen called");
    }
}

class AssignStmtNode extends StmtNode {
    public AssignStmtNode(AssignNode assign) {
        myAssign = assign;
    }
    /**
     * Generate MIPS code for this node
     */
    public void codegen(String eLbl) {
      System.out.println("AssignStmtNode's codegen called");
      // codegen on myAssign, pop junk data
      myAssign.codegen();
      Codegen.genPop("$t0");
    }
    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child
     */
    public void nameAnalysis(SymTable symTab) {
        myAssign.nameAnalysis(symTab);
    }
    
    /**
     * typeCheck
     */
    public void typeCheck(Type retType) {
        myAssign.typeCheck();
    }
        
    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        myAssign.unparse(p, -1); // no parentheses
        p.println(";");
    }

    // 1 kid
    private AssignNode myAssign;
}

class PostIncStmtNode extends StmtNode {
    public PostIncStmtNode(ExpNode exp) {
        myExp = exp;
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child
     */
    public void nameAnalysis(SymTable symTab) {
        myExp.nameAnalysis(symTab);
    }
    
    /**
     * typeCheck
     */
    public void typeCheck(Type retType) {
        Type type = myExp.typeCheck();
        
        if (!type.isErrorType() && !type.isIntType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Arithmetic operator applied to non-numeric operand");
        }
    }
        
    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        myExp.unparse(p, 0);
        p.println("++;");
    }

    // 1 kid
    private ExpNode myExp;
}

class PostDecStmtNode extends StmtNode {
    public PostDecStmtNode(ExpNode exp) {
        myExp = exp;
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child
     */
    public void nameAnalysis(SymTable symTab) {
        myExp.nameAnalysis(symTab);
    }
    
    /**
     * typeCheck
     */
    public void typeCheck(Type retType) {
        Type type = myExp.typeCheck();
        
        if (!type.isErrorType() && !type.isIntType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Arithmetic operator applied to non-numeric operand");
        }
    }
        
    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        myExp.unparse(p, 0);
        p.println("--;");
    }
    
    // 1 kid
    private ExpNode myExp;
}

class ReadStmtNode extends StmtNode {
    public ReadStmtNode(ExpNode e) {
        myExp = e;
    }
    /**
     * Generate MIPS code for this node
     */
    public void codegen(String eLbl) {
      System.out.println("ReadStmtNode's codegen called");
      IdNode id = (IdNode)myExp;
      
      // Perform syscall to get input value into $v0
      Codegen.generate("li","$v0",5);
      Codegen.generate("syscall");

      // Store that into mem location of id
      if (id.sym().isGlbl())
         Codegen.generate("sw","$v0","_"+id.name());
      else
         Codegen.generateIndexed("sw","$v0","$fp",id.sym().getOffset());
    }
    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child
     */
    public void nameAnalysis(SymTable symTab) {
        myExp.nameAnalysis(symTab);
    }    
 
    /**
     * typeCheck
     */
    public void typeCheck(Type retType) {
        Type type = myExp.typeCheck();
        
        if (type.isFnType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Attempt to read a function");
        }
        
        if (type.isStructDefType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Attempt to read a struct name");
        }
        
        if (type.isStructType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Attempt to read a struct variable");
        }
    }
    
    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        p.print("cin >> ");
        myExp.unparse(p, 0);
        p.println(";");
    }

    // 1 kid (actually can only be an IdNode or an ArrayExpNode)
    private ExpNode myExp;
}

class WriteStmtNode extends StmtNode {
    //type of exp being written
    private Type writeType;
    public Type getWriteType(){
        return writeType;
    }
    public WriteStmtNode(ExpNode exp) {
        myExp = exp;
    }
    /**
     * Generate MIPS code for this node
     */
    public void codegen(String eLbl) {
      System.out.println("WriteStmtNode's codegen called");
      myExp.codegen();
      Codegen.genPop("$a0");

      // put correct value into $v0 depending on type(int & bool get 1, str 4)
      if (writeType.isIntType())
         Codegen.generate("li","$v0",1);
      if (writeType.isBoolType())
         Codegen.generate("li","$v0",1);
      if (writeType.isStringType())
         Codegen.generate("li", "$v0", 4);
      
      // Perform syscall
      Codegen.generate("syscall");
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child
     */
    public void nameAnalysis(SymTable symTab) {
        myExp.nameAnalysis(symTab);
    }

    /**
     * typeCheck
     */
    public void typeCheck(Type retType) {
        Type type = myExp.typeCheck();
        this.writeType = type;
        if (type.isFnType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Attempt to write a function");
        }
        
        if (type.isStructDefType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Attempt to write a struct name");
        }
        
        if (type.isStructType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Attempt to write a struct variable");
        }
        
        if (type.isVoidType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Attempt to write void");
        }
    }
        
    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        p.print("cout << ");
        myExp.unparse(p, 0);
        p.println(";");
    }

    // 1 kid
    private ExpNode myExp;
}

class IfStmtNode extends StmtNode {
    public IfStmtNode(ExpNode exp, DeclListNode dlist, StmtListNode slist) {
        myDeclList = dlist;
        myExp = exp;
        myStmtList = slist;
    }
    
    /**
     * Generates MIPS code for this node
     */
    public void codegen(String eLbl) {
      System.out.println("IfStmtNode's codegen called");
      // no need to gen code for decllist, get false label
      String fLbl = Codegen.nextLabel();
      // evaluate exp
      myExp.codegen();
      Codegen.genPop("$t0");
      // branch on exp evals to false (numeric approach!)
      Codegen.generate("beq","$t0","0",fLbl);
      // generate body's code & false label
      myStmtList.codegen(eLbl);
      Codegen.genLabel(fLbl, "endif");
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * - process the condition
     * - enter a new scope
     * - process the decls and stmts
     * - exit the scope
     */
    public void nameAnalysis(SymTable symTab) {
        myExp.nameAnalysis(symTab);
        symTab.addScope();
        myDeclList.nameAnalysis(symTab);
        myStmtList.nameAnalysis(symTab);
        try {
            symTab.removeScope();
        } catch (EmptySymTableException ex) {
            System.err.println("Unexpected EmptySymTableException " +
                               " in IfStmtNode.nameAnalysis");
            System.exit(-1);        
        }
    }
    
     /**
     * typeCheck
     */
    public void typeCheck(Type retType) {
        Type type = myExp.typeCheck();
        
        if (!type.isErrorType() && !type.isBoolType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Non-bool expression used as an if condition");        
        }
        
        myStmtList.typeCheck(retType);
    }
       
    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        p.print("if (");
        myExp.unparse(p, 0);
        p.println(") {");
        myDeclList.unparse(p, indent+4);
        myStmtList.unparse(p, indent+4);
        doIndent(p, indent);
        p.println("}");
    }

    // e kids
    private ExpNode myExp;
    private DeclListNode myDeclList;
    private StmtListNode myStmtList;
}

class IfElseStmtNode extends StmtNode {
    public IfElseStmtNode(ExpNode exp, DeclListNode dlist1,
                          StmtListNode slist1, DeclListNode dlist2,
                          StmtListNode slist2) {
        myExp = exp;
        myThenDeclList = dlist1;
        myThenStmtList = slist1;
        myElseDeclList = dlist2;
        myElseStmtList = slist2;
    }
    
    /**
     * Generate MIPS code for this node
     */
    public void codegen(String eLbl) {
      System.out.println("IfElseStmtNode's codegen called");
      // get false & done labels
      String fLbl = Codegen.nextLabel();
      String dLbl = Codegen.nextLabel();
      
      // Evaluate expression
      myExp.codegen();
      Codegen.genPop("$t0");
      
      // branch on exp evals to false (numeric approach!)
      Codegen.generate("beq","$t0","0",fLbl);
      
      // generate true body's code & unconditional br to done label
      myThenStmtList.codegen(eLbl);
      Codegen.generate("b",dLbl);
      
      // generate false label, false body's code & done label
      Codegen.genLabel(fLbl, "else block");
      myElseStmtList.codegen(eLbl);
      Codegen.genLabel(dLbl, "end if-else");
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * - process the condition
     * - enter a new scope
     * - process the decls and stmts of then
     * - exit the scope
     * - enter a new scope
     * - process the decls and stmts of else
     * - exit the scope
     */
    public void nameAnalysis(SymTable symTab) {
        myExp.nameAnalysis(symTab);
        symTab.addScope();
        myThenDeclList.nameAnalysis(symTab);
        myThenStmtList.nameAnalysis(symTab);
        try {
            symTab.removeScope();
        } catch (EmptySymTableException ex) {
            System.err.println("Unexpected EmptySymTableException " +
                               " in IfStmtNode.nameAnalysis");
            System.exit(-1);        
        }
        symTab.addScope();
        myElseDeclList.nameAnalysis(symTab);
        myElseStmtList.nameAnalysis(symTab);
        try {
            symTab.removeScope();
        } catch (EmptySymTableException ex) {
            System.err.println("Unexpected EmptySymTableException " +
                               " in IfStmtNode.nameAnalysis");
            System.exit(-1);        
        }
    }
    
    /**
     * typeCheck
     */
    public void typeCheck(Type retType) {
        Type type = myExp.typeCheck();
        
        if (!type.isErrorType() && !type.isBoolType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Non-bool expression used as an if condition");        
        }
        
        myThenStmtList.typeCheck(retType);
        myElseStmtList.typeCheck(retType);
    }
        
    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        p.print("if (");
        myExp.unparse(p, 0);
        p.println(") {");
        myThenDeclList.unparse(p, indent+4);
        myThenStmtList.unparse(p, indent+4);
        doIndent(p, indent);
        p.println("}");
        doIndent(p, indent);
        p.println("else {");
        myElseDeclList.unparse(p, indent+4);
        myElseStmtList.unparse(p, indent+4);
        doIndent(p, indent);
        p.println("}");        
    }

    // 5 kids
    private ExpNode myExp;
    private DeclListNode myThenDeclList;
    private StmtListNode myThenStmtList;
    private StmtListNode myElseStmtList;
    private DeclListNode myElseDeclList;
}

class WhileStmtNode extends StmtNode {
    public WhileStmtNode(ExpNode exp, DeclListNode dlist, StmtListNode slist) {
        myExp = exp;
        myDeclList = dlist;
        myStmtList = slist;
    }

    /**
     * Generate MIPS code for this node
     */
    public void codegen(String eLbl) {
      // get condition & done labels
      String cLbl = Codegen.nextLabel();
      String dLbl = Codegen.nextLabel();

      // gen clabel first (will need to reevaluate exp each loop iteration)
      Codegen.genLabel(cLbl, "while condition");
      myExp.codegen();
      Codegen.genPop("$t0");
      Codegen.generate("beq","$t0","0",dLbl);

      // generate body's code, unconditional br back to elbl & gen dLbl
      myStmtList.codegen(eLbl);
      Codegen.generate("b",cLbl);
      Codegen.genLabel(dLbl, "end while");
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * - process the condition
     * - enter a new scope
     * - process the decls and stmts
     * - exit the scope
     */
    public void nameAnalysis(SymTable symTab) {
        myExp.nameAnalysis(symTab);
        symTab.addScope();
        myDeclList.nameAnalysis(symTab);
        myStmtList.nameAnalysis(symTab);
        try {
            symTab.removeScope();
        } catch (EmptySymTableException ex) {
            System.err.println("Unexpected EmptySymTableException " +
                               " in IfStmtNode.nameAnalysis");
            System.exit(-1);        
        }
    }
    
    /**
     * typeCheck
     */
    public void typeCheck(Type retType) {
        Type type = myExp.typeCheck();
        
        if (!type.isErrorType() && !type.isBoolType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Non-bool expression used as a while condition");        
        }
        
        myStmtList.typeCheck(retType);
    }
        
    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        p.print("while (");
        myExp.unparse(p, 0);
        p.println(") {");
        myDeclList.unparse(p, indent+4);
        myStmtList.unparse(p, indent+4);
        doIndent(p, indent);
        p.println("}");
    }

    // 3 kids
    private ExpNode myExp;
    private DeclListNode myDeclList;
    private StmtListNode myStmtList;
}

class CallStmtNode extends StmtNode {
    public CallStmtNode(CallExpNode call) {
        myCall = call;
    }

    /**
     * Generate MIPS code for this node
     */
    public void codegen(String eLbl) {
      System.out.println("CallStmtNode's codegen called");
      // Generate code for mycall, pop junk data to restore stack
      myCall.codegen();
      Codegen.genPop("$t0");
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child
     */
    public void nameAnalysis(SymTable symTab) {
        myCall.nameAnalysis(symTab);
    }
    
    /**
     * typeCheck
     */
    public void typeCheck(Type retType) {
        myCall.typeCheck();
    }
    
    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        myCall.unparse(p, indent);
        p.println(";");
    }

    // 1 kid
    private CallExpNode myCall;
}

class ReturnStmtNode extends StmtNode {
    public ReturnStmtNode(ExpNode exp) {
        myExp = exp;
    }
    
    /**
     * Generate MIPS code for this node
     */
    public void codegen(String eLbl) {
      // Put return value into $v0
      if (myExp != null) {
         myExp.codegen();
         Codegen.genPop("$v0");
      }
      // Exit function unconditionally
      Codegen.generate("b",eLbl);
    }
    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child,
     * if it has one
     */
    public void nameAnalysis(SymTable symTab) {
        if (myExp != null) {
            myExp.nameAnalysis(symTab);
        }
    }

    /**
     * typeCheck
     */
    public void typeCheck(Type retType) {
        if (myExp != null) {  // return value given
            Type type = myExp.typeCheck();
            
            if (retType.isVoidType()) {
                ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                             "Return with a value in a void function");                
            }
            
            else if (!retType.isErrorType() && !type.isErrorType() && !retType.equals(type)){
                ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                             "Bad return value");
            }
        }
        
        else {  // no return value given -- ok if this is a void function
            if (!retType.isVoidType()) {
                ErrMsg.fatal(0, 0, "Missing return value");                
            }
        }
        
    }
    
    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        p.print("return");
        if (myExp != null) {
            p.print(" ");
            myExp.unparse(p, 0);
        }
        p.println(";");
    }

    // 1 kid
    private ExpNode myExp; // possibly null
}

// **********************************************************************
// ExpNode and its subclasses
// **********************************************************************

abstract class ExpNode extends ASTnode {
    /**
     * Default version for nodes with no names
     */
    public void nameAnalysis(SymTable symTab) { }
    
    abstract public Type typeCheck();
    abstract public int lineNum();
    abstract public int charNum();
    // default codegen method
    public void codegen() { }
}

class IntLitNode extends ExpNode {
    public IntLitNode(int lineNum, int charNum, int intVal) {
        myLineNum = lineNum;
        myCharNum = charNum;
        myIntVal = intVal;
    }
    
    /**
     * Generate MIPS code for this node
     */
    public void codegen() {
      // Push this node's value onto the stack
      Codegen.generate("li","$t0",myIntVal);
      Codegen.genPush("$t0");
    }

    /**
     * Return the line number for this literal.
     */
    public int lineNum() {
        return myLineNum;
    }
    
    /**
     * Return the char number for this literal.
     */
    public int charNum() {
        return myCharNum;
    }
        
    /**
     * typeCheck
     */
    public Type typeCheck() {
        return new IntType();
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print(myIntVal);
    }

    private int myLineNum;
    private int myCharNum;
    private int myIntVal;
}

class StringLitNode extends ExpNode {
    public StringLitNode(int lineNum, int charNum, String strVal) {
        myLineNum = lineNum;
        myCharNum = charNum;
        myStrVal = strVal;
    }
    
    /**
     * Generate MIPS code for this node
     */
    public void codegen() {
      // Check that lblmap exists, if not make new one
      if (lblMap == null)
         lblMap = new HashMap<String,String>();
      
      // Check hashmap to see if our string is already in data
      String lbl = lblMap.get(myStrVal);
      if (lbl == null) {
         // Get next unique label and put that in our hashmap
         lbl = Codegen.nextLabel();
         lblMap.put(myStrVal, lbl);
         // Store data for this string
         Codegen.generate(".data");
         String str = ".asciiz " + myStrVal;
         Codegen.generateLabeled(lbl,str,"");
         Codegen.generate(".text");
      }

      // Push lbl's address onto stack   
      Codegen.generate("la","$t0",lbl);
      Codegen.genPush("$t0");
    }

    /**
     * Return the line number for this literal.
     */
    public int lineNum() {
        return myLineNum;
    }
    
    /**
     * Return the char number for this literal.
     */
    public int charNum() {
        return myCharNum;
    }
    
    /**
     * typeCheck
     */
    public Type typeCheck() {
        return new StringType();
    }
        
    public void unparse(PrintWriter p, int indent) {
        p.print(myStrVal);
    }

    private int myLineNum;
    private int myCharNum;
    private String myStrVal;
    protected static HashMap<String,String> lblMap;
}

class TrueNode extends ExpNode {
    public TrueNode(int lineNum, int charNum) {
        myLineNum = lineNum;
        myCharNum = charNum;
    }

    /**
     * Generate MIPS code for this node
     */
    public void codegen() {
      // Push true value onto the stack
      Codegen.generate("li","$t0",1);
      Codegen.genPush("$t0");
    }

    /**
     * Return the line number for this literal.
     */
    public int lineNum() {
        return myLineNum;
    }
    
    /**
     * Return the char number for this literal.
     */
    public int charNum() {
        return myCharNum;
    }
    
    /**
     * typeCheck
     */
    public Type typeCheck() {
        return new BoolType();
    }
        
    public void unparse(PrintWriter p, int indent) {
        p.print("true");
    }

    private int myLineNum;
    private int myCharNum;
}

class FalseNode extends ExpNode {
    public FalseNode(int lineNum, int charNum) {
        myLineNum = lineNum;
        myCharNum = charNum;
    }
    /**
     * Generate MIPS code for this node
     */
    public void codegen() {
      // Push false value onto the stack
      Codegen.generate("li","$t0",0);
      Codegen.genPush("$t0");
    }

    /**
     * Return the line number for this literal.
     */
    public int lineNum() {
        return myLineNum;
    }
    
    /**
     * Return the char number for this literal.
     */
    public int charNum() {
        return myCharNum;
    }

    /**
     * typeCheck
     */
    public Type typeCheck() {
        return new BoolType();
    }
        
    public void unparse(PrintWriter p, int indent) {
        p.print("false");
    }

    private int myLineNum;
    private int myCharNum;
}

class IdNode extends ExpNode {
    public IdNode(int lineNum, int charNum, String strVal) {
        myLineNum = lineNum;
        myCharNum = charNum;
        myStrVal = strVal;
    }
    /**
     * Generate a jump and link for this id node
     */
    public void genJAL() {
      System.out.println("IdNode's genJAL called");
      String ent = ((FnSym)mySym).entry();
      Codegen.generate("jal", ent);
    }
    /**
     * Generate address for this id node
     */
    public void genAddr() {
      System.out.println("IdNode's genAddr called");
      // Calculate address & push on stack
      if (mySym.isGlbl()) {
         Codegen.generate("la","$t0","_" + myStrVal);
      } else {
         Codegen.generateIndexed("la","$t0","$fp",mySym.getOffset());
      }
      Codegen.genPush("$t0");
    }
    
    /**
     * Generate MIPS code for this node
     */
    public void codegen() {
      System.out.println("IdNode's codegen called");
      // Load data from this var's address & push value
      if (mySym.isGlbl()) {
         Codegen.generate("lw","$t0","_" + myStrVal);
      } else {
         Codegen.generateIndexed("lw","$t0","$fp",mySym.getOffset());
      }
      Codegen.genPush("$t0");
    }
    /**
     * Link the given symbol to this ID.
     */
    public void link(Sym sym) {
        mySym = sym;
    }
    
    /**
     * Return the name of this ID.
     */
    public String name() {
        return myStrVal;
    }
    
    /**
     * Return the symbol associated with this ID.
     */
    public Sym sym() {
        return mySym;
    }
    
    /**
     * Return the line number for this ID.
     */
    public int lineNum() {
        return myLineNum;
    }
    
    /**
     * Return the char number for this ID.
     */
    public int charNum() {
        return myCharNum;
    }    
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * - check for use of undeclared name
     * - if ok, link to symbol table entry
     */
    public void nameAnalysis(SymTable symTab) {
        Sym sym = symTab.lookupGlobal(myStrVal);
        if (sym == null) {
            ErrMsg.fatal(myLineNum, myCharNum, "Undeclared identifier");
        } else {
            link(sym);
        }
    }
 
    /**
     * typeCheck
     */
    public Type typeCheck() {
        if (mySym != null) {
            return mySym.getType();
        } 
        else {
            System.err.println("ID with null sym field in IdNode.typeCheck");
            System.exit(-1);
        }
        return null;
    }
           
    public void unparse(PrintWriter p, int indent) {
        p.print(myStrVal);
        if (mySym != null) {
            p.print("(" + mySym + ")");
        }
    }

    private int myLineNum;
    private int myCharNum;
    private String myStrVal;
    private Sym mySym;
}

class DotAccessExpNode extends ExpNode {
    //NO CODEGEN
    public DotAccessExpNode(ExpNode loc, IdNode id) {
        myLoc = loc;    
        myId = id;
        mySym = null;
    }

    /**
     * Return the symbol associated with this dot-access node.
     */
    public Sym sym() {
        return mySym;
    }    
    
    /**
     * Return the line number for this dot-access node. 
     * The line number is the one corresponding to the RHS of the dot-access.
     */
    public int lineNum() {
        return myId.lineNum();
    }
    
    /**
     * Return the char number for this dot-access node.
     * The char number is the one corresponding to the RHS of the dot-access.
     */
    public int charNum() {
        return myId.charNum();
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * - process the LHS of the dot-access
     * - process the RHS of the dot-access
     * - if the RHS is of a struct type, set the sym for this node so that
     *   a dot-access "higher up" in the AST can get access to the symbol
     *   table for the appropriate struct definition
     */
    public void nameAnalysis(SymTable symTab) {
        badAccess = false;
        SymTable structSymTab = null; // to lookup RHS of dot-access
        Sym sym = null;
        
        myLoc.nameAnalysis(symTab);  // do name analysis on LHS
        
        // if myLoc is really an ID, then sym will be a link to the ID's symbol
        if (myLoc instanceof IdNode) {
            IdNode id = (IdNode)myLoc;
            sym = id.sym();
            
            // check ID has been declared to be of a struct type
            
            if (sym == null) { // ID was undeclared
                badAccess = true;
            }
            else if (sym instanceof StructSym) { 
                // get symbol table for struct type
                Sym tempSym = ((StructSym)sym).getStructType().sym();
                structSymTab = ((StructDefSym)tempSym).getSymTable();
            } 
            else {  // LHS is not a struct type
                ErrMsg.fatal(id.lineNum(), id.charNum(), 
                             "Dot-access of non-struct type");
                badAccess = true;
            }
        }
        
        // if myLoc is really a dot-access (i.e., myLoc was of the form
        // LHSloc.RHSid), then sym will either be
        // null - indicating RHSid is not of a struct type, or
        // a link to the Sym for the struct type RHSid was declared to be
        else if (myLoc instanceof DotAccessExpNode) {
            DotAccessExpNode loc = (DotAccessExpNode)myLoc;
            
            if (loc.badAccess) {  // if errors in processing myLoc
                badAccess = true; // don't continue proccessing this dot-access
            }
            else { //  no errors in processing myLoc
                sym = loc.sym();

                if (sym == null) {  // no struct in which to look up RHS
                    ErrMsg.fatal(loc.lineNum(), loc.charNum(), 
                                 "Dot-access of non-struct type");
                    badAccess = true;
                }
                else {  // get the struct's symbol table in which to lookup RHS
                    if (sym instanceof StructDefSym) {
                        structSymTab = ((StructDefSym)sym).getSymTable();
                    }
                    else {
                        System.err.println("Unexpected Sym type in DotAccessExpNode");
                        System.exit(-1);
                    }
                }
            }

        }
        
        else { // don't know what kind of thing myLoc is
            System.err.println("Unexpected node type in LHS of dot-access");
            System.exit(-1);
        }
        
        // do name analysis on RHS of dot-access in the struct's symbol table
        if (!badAccess) {
        
            sym = structSymTab.lookupGlobal(myId.name()); // lookup
            if (sym == null) { // not found - RHS is not a valid field name
                ErrMsg.fatal(myId.lineNum(), myId.charNum(), 
                             "Invalid struct field name");
                badAccess = true;
            }
            
            else {
                myId.link(sym);  // link the symbol
                // if RHS is itself as struct type, link the symbol for its struct 
                // type to this dot-access node (to allow chained dot-access)
                if (sym instanceof StructSym) {
                    mySym = ((StructSym)sym).getStructType().sym();
                }
            }
        }
    }    
 
    /**
     * typeCheck
     */
    public Type typeCheck() {
        return myId.typeCheck();
    }
    
    public void unparse(PrintWriter p, int indent) {
        myLoc.unparse(p, 0);
        p.print(".");
        myId.unparse(p, 0);
    }

    // 2 kids
    private ExpNode myLoc;    
    private IdNode myId;
    private Sym mySym;          // link to Sym for struct type
    private boolean badAccess;  // to prevent multiple, cascading errors
}

class AssignNode extends ExpNode {
    public AssignNode(ExpNode lhs, ExpNode exp) {
        myLhs = lhs;
        myExp = exp;
    }
    /**
     * Generate MIPS code for this node
     */
    public void codegen() {
      System.out.println("AssignNode's codegen called");
      
      // Put myExp's value into $t0
      myExp.codegen();
      Codegen.genPop("$t0");
      
      // Get offset from lhs 
      IdNode id = ((IdNode)myLhs);
      if (id.sym().isGlbl()) 
         Codegen.generate("sw","$t0","_" + id.name());
      else
         Codegen.generateIndexed("sw","$t0","$fp",id.sym().getOffset());

      // Push value onto stack
      Codegen.genPush("$t0");
    }
    /**
     * Return the line number for this assignment node. 
     * The line number is the one corresponding to the left operand.
     */
    public int lineNum() {
        return myLhs.lineNum();
    }
    
    /**
     * Return the char number for this assignment node.
     * The char number is the one corresponding to the left operand.
     */
    public int charNum() {
        return myLhs.charNum();
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's 
     * two children
     */
    public void nameAnalysis(SymTable symTab) {
        myLhs.nameAnalysis(symTab);
        myExp.nameAnalysis(symTab);
    }
 
    /**
     * typeCheck
     */
    public Type typeCheck() {
        Type typeLhs = myLhs.typeCheck();
        Type typeExp = myExp.typeCheck();
        Type retType = typeLhs;
        
        if (typeLhs.isFnType() && typeExp.isFnType()) {
            ErrMsg.fatal(lineNum(), charNum(), "Function assignment");
            retType = new ErrorType();
        }
        
        if (typeLhs.isStructDefType() && typeExp.isStructDefType()) {
            ErrMsg.fatal(lineNum(), charNum(), "Struct name assignment");
            retType = new ErrorType();
        }
        
        if (typeLhs.isStructType() && typeExp.isStructType()) {
            ErrMsg.fatal(lineNum(), charNum(), "Struct variable assignment");
            retType = new ErrorType();
        }        
        
        if (!typeLhs.equals(typeExp) && !typeLhs.isErrorType() && !typeExp.isErrorType()) {
            ErrMsg.fatal(lineNum(), charNum(), "Type mismatch");
            retType = new ErrorType();
        }
        
        if (typeLhs.isErrorType() || typeExp.isErrorType()) {
            retType = new ErrorType();
        }
        
        return retType;
    }
    
    public void unparse(PrintWriter p, int indent) {
        if (indent != -1)  p.print("(");
        myLhs.unparse(p, 0);
        p.print(" = ");
        myExp.unparse(p, 0);
        if (indent != -1)  p.print(")");
    }

    // 2 kids
    private ExpNode myLhs;
    private ExpNode myExp;
}

class CallExpNode extends ExpNode {
    public CallExpNode(IdNode name, ExpListNode elist) {
        myId = name;
        myExpList = elist;
    }
    /**
     * Generate MIPS code for this node
     */
    public void codegen() {
      // Push parameters onto stack
      myExpList.codegen();
      // Generate jump & link from id node
      myId.genJAL();
      // Push contents of $v0 onto stack (in case of void, just pushes junk
      // data, it's okay since we'll pop it later & it can't be used in an
      // expression evaluation)
      Codegen.genPush("$v0");
    }
    public CallExpNode(IdNode name) {
        myId = name;
        myExpList = new ExpListNode(new LinkedList<ExpNode>());
    }

    /**
     * Return the line number for this call node. 
     * The line number is the one corresponding to the function name.
     */
    public int lineNum() {
        return myId.lineNum();
    }
    
    /**
     * Return the char number for this call node.
     * The char number is the one corresponding to the function name.
     */
    public int charNum() {
        return myId.charNum();
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's 
     * two children
     */
    public void nameAnalysis(SymTable symTab) {
        myId.nameAnalysis(symTab);
        myExpList.nameAnalysis(symTab);
    }  
      
    /**
     * typeCheck
     */
    public Type typeCheck() {
        if (!myId.typeCheck().isFnType()) {  
            ErrMsg.fatal(myId.lineNum(), myId.charNum(), 
                         "Attempt to call a non-function");
            return new ErrorType();
        }
        
        FnSym fnSym = (FnSym)(myId.sym());
        
        if (fnSym == null) {
            System.err.println("null sym for Id in CallExpNode.typeCheck");
            System.exit(-1);
        }
        
        if (myExpList.size() != fnSym.getNumParams()) {
            ErrMsg.fatal(myId.lineNum(), myId.charNum(), 
                         "Function call with wrong number of args");
            return fnSym.getReturnType();
        }
        
        myExpList.typeCheck(fnSym.getParamTypes());
        return fnSym.getReturnType();
    }
        
    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
        myId.unparse(p, 0);
        p.print("(");
        if (myExpList != null) {
            myExpList.unparse(p, 0);
        }
        p.print(")");
    }

    // 2 kids
    private IdNode myId;
    private ExpListNode myExpList;  // possibly null
}

abstract class UnaryExpNode extends ExpNode {
    public UnaryExpNode(ExpNode exp) {
        myExp = exp;
    }
    
    /**
     * Return the line number for this unary expression node. 
     * The line number is the one corresponding to the  operand.
     */
    public int lineNum() {
        return myExp.lineNum();
    }
    
    /**
     * Return the char number for this unary expression node.
     * The char number is the one corresponding to the  operand.
     */
    public int charNum() {
        return myExp.charNum();
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child
     */
    public void nameAnalysis(SymTable symTab) {
        myExp.nameAnalysis(symTab);
    }
    
    // one child
    protected ExpNode myExp;
}

abstract class BinaryExpNode extends ExpNode {
    public BinaryExpNode(ExpNode exp1, ExpNode exp2) {
        myExp1 = exp1;
        myExp2 = exp2;
    }
    /**
     * Generate MIPS code for this node
     */
    public void codegen() {
      // eval lh exp & rh exp, pop into $t0 & $t1
      myExp1.codegen();
      myExp2.codegen();
      Codegen.genPop("$t1");
      Codegen.genPop("$t0");
      Codegen.generate(myOp, "$t0","$t0", "$t1");
      Codegen.genPush("$t0");
    }

    /**
     * Return the line number for this binary expression node. 
     * The line number is the one corresponding to the left operand.
     */
    public int lineNum() {
        return myExp1.lineNum();
    }
    
    /**
     * Return the char number for this binary expression node.
     * The char number is the one corresponding to the left operand.
     */
    public int charNum() {
        return myExp1.charNum();
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's 
     * two children
     */
    public void nameAnalysis(SymTable symTab) {
        myExp1.nameAnalysis(symTab);
        myExp2.nameAnalysis(symTab);
    }
    
    // two kids
    protected ExpNode myExp1;
    protected ExpNode myExp2;
    protected String myOp;
}

// **********************************************************************
// Subclasses of UnaryExpNode
// **********************************************************************

class UnaryMinusNode extends UnaryExpNode {
    public UnaryMinusNode(ExpNode exp) {
        super(exp);
    }
    /**
     * Generate MIPS code for this node
     */
    public void codegen() {
      System.out.println("UnaryMinusNode's codegen called");
      
      // Evaluate expression & pop into $t0
      myExp.codegen();
      Codegen.genPop("$t0");
      
      // Subtract $t0 from 0
      Codegen.generate("li", "$t1", 0);
      Codegen.generate("sub","$t0","$t1","$t0");

      // Push result
      Codegen.genPush("$t0");
    }
    /**
     * typeCheck
     */
    public Type typeCheck() {
        Type type = myExp.typeCheck();
        Type retType = new IntType();
        
        if (!type.isErrorType() && !type.isIntType()) {
            ErrMsg.fatal(lineNum(), charNum(),
                         "Arithmetic operator applied to non-numeric operand");
            retType = new ErrorType();
        }
        
        if (type.isErrorType()) {
            retType = new ErrorType();
        }
        
        return retType;
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(-");
        myExp.unparse(p, 0);
        p.print(")");
    }
}

class NotNode extends UnaryExpNode {
    public NotNode(ExpNode exp) {
        super(exp);
    }
    /**
     * Generate MIPS code for this node
     */
    public void codegen() {
      System.out.println("NotNode's codegen called");

      // eval myExp and put in $t0
      myExp.codegen();
      Codegen.genPop("$t0");
      
      // Don't want to use bitwise not, instead do 1-$t0
      Codegen.generate("li","$t1",1);
      Codegen.generate("sub","$t0","$t1","$t0");
      
      // Push result onto stack
      Codegen.genPush("$t0");
    }
    /**
     * typeCheck
     */
    public Type typeCheck() {
        Type type = myExp.typeCheck();
        Type retType = new BoolType();
        
        if (!type.isErrorType() && !type.isBoolType()) {
            ErrMsg.fatal(lineNum(), charNum(),
                         "Logical operator applied to non-bool operand");
            retType = new ErrorType();
        }
        
        if (type.isErrorType()) {
            retType = new ErrorType();
        }
        
        return retType;
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(!");
        myExp.unparse(p, 0);
        p.print(")");
    }
}

// **********************************************************************
// Subclasses of BinaryExpNode
// **********************************************************************

abstract class ArithmeticExpNode extends BinaryExpNode {
    public ArithmeticExpNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    /**
     * typeCheck
     */
    public Type typeCheck() {
        Type type1 = myExp1.typeCheck();
        Type type2 = myExp2.typeCheck();
        Type retType = new IntType();
        
        if (!type1.isErrorType() && !type1.isIntType()) {
            ErrMsg.fatal(myExp1.lineNum(), myExp1.charNum(),
                         "Arithmetic operator applied to non-numeric operand");
            retType = new ErrorType();
        }
        
        if (!type2.isErrorType() && !type2.isIntType()) {
            ErrMsg.fatal(myExp2.lineNum(), myExp2.charNum(),
                         "Arithmetic operator applied to non-numeric operand");
            retType = new ErrorType();
        }
        
        if (type1.isErrorType() || type2.isErrorType()) {
            retType = new ErrorType();
        }
        
        return retType;
    }
}

abstract class LogicalExpNode extends BinaryExpNode {
    public LogicalExpNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    /**
     * Generate MIPS code for this node
     */
    public void codegen() {
      // Get done label
      String dLbl = Codegen.nextLabel();

      // Evaluate left expression, on myOp, it's done
      myExp1.codegen();
      Codegen.genPop("$t0");
      Codegen.generate("beq","$t0",myOp,dLbl);

      // Code for $t0 didn't eval to myOp (whole exp evaluates to rh exp)
      myExp2.codegen();
      Codegen.genPop("$t0");

      // Make done lbl and push $t0 as the result
      Codegen.genLabel(dLbl);
      Codegen.genPush("$t0");
    }

    /**
     * typeCheck
     */
    public Type typeCheck() {
        Type type1 = myExp1.typeCheck();
        Type type2 = myExp2.typeCheck();
        Type retType = new BoolType();
        
        if (!type1.isErrorType() && !type1.isBoolType()) {
            ErrMsg.fatal(myExp1.lineNum(), myExp1.charNum(),
                         "Logical operator applied to non-bool operand");
            retType = new ErrorType();
        }
        
        if (!type2.isErrorType() && !type2.isBoolType()) {
            ErrMsg.fatal(myExp2.lineNum(), myExp2.charNum(),
                         "Logical operator applied to non-bool operand");
            retType = new ErrorType();
        }
        
        if (type1.isErrorType() || type2.isErrorType()) {
            retType = new ErrorType();
        }
        
        return retType;
    }
}

abstract class EqualityExpNode extends BinaryExpNode {
    public EqualityExpNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    /**
     * Generate MIPS code for this node
     */
    public void codegen() {
      System.out.println("EqualityExpNode's codegen called");
      // Get true & done labels
      String tLbl = Codegen.nextLabel();
      String dLbl = Codegen.nextLabel();

      // Eval both exp's and pop them from stack
      myExp1.codegen();
      myExp2.codegen();
      Codegen.genPop("$t1");
      Codegen.genPop("$t0");

      // Jump to tlbl on myOp true & put 1 in t0, otherwise put 0 & jump to done
      Codegen.generate(myOp,"$t0","$t1",tLbl);
      Codegen.generate("li","$t0",0);
      Codegen.generate("b", dLbl);
      Codegen.genLabel(tLbl);

      // myOp is true, so put 1 in $t0
      Codegen.generate("li","$t0",1);
      Codegen.genLabel(dLbl);
      Codegen.genPush("$t0");
    }

    /**
     * typeCheck
     */
    public Type typeCheck() {
        Type type1 = myExp1.typeCheck();
        Type type2 = myExp2.typeCheck();
        Type retType = new BoolType();
        
        if (type1.isVoidType() && type2.isVoidType()) {
            ErrMsg.fatal(lineNum(), charNum(),
                         "Equality operator applied to void functions");
            retType = new ErrorType();
        }
        
        if (type1.isFnType() && type2.isFnType()) {
            ErrMsg.fatal(lineNum(), charNum(),
                         "Equality operator applied to functions");
            retType = new ErrorType();
        }
        
        if (type1.isStructDefType() && type2.isStructDefType()) {
            ErrMsg.fatal(lineNum(), charNum(),
                         "Equality operator applied to struct names");
            retType = new ErrorType();
        }
        
        if (type1.isStructType() && type2.isStructType()) {
            ErrMsg.fatal(lineNum(), charNum(),
                         "Equality operator applied to struct variables");
            retType = new ErrorType();
        }        
        
        if (!type1.equals(type2) && !type1.isErrorType() && !type2.isErrorType()) {
            ErrMsg.fatal(lineNum(), charNum(),
                         "Type mismatch");
            retType = new ErrorType();
        }
        
        if (type1.isErrorType() || type2.isErrorType()) {
            retType = new ErrorType();
        }
        
        return retType;
    }
}

abstract class RelationalExpNode extends BinaryExpNode {
    public RelationalExpNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    /**
     * Generate MIPS code for this node
     */
    public void codegen() {
      System.out.println("RelationalExpNode's codegen called");
      // Get true & done labels
      String tLbl = Codegen.nextLabel();
      String dLbl = Codegen.nextLabel();

      // Eval both exp's, pop them from stack, subtract & put in $t0
      myExp1.codegen();
      myExp2.codegen();
      Codegen.genPop("$t1");
      Codegen.genPop("$t0");
      Codegen.generate("sub","$t0","$t0","$t1");

      // Jump to tLbl on op & put 1 in t0, otherwise put 0 & jump to done
      Codegen.generate(myOp,"$t0",tLbl);
      Codegen.generate("li","$t0",0);
      Codegen.generate("b", dLbl);
      Codegen.genLabel(tLbl);

      // Op is true, so put 1 in $t0
      Codegen.generate("li","$t0",1);
      Codegen.genLabel(dLbl);
      Codegen.genPush("$t0");
    }
    
    /**
     * typeCheck
     */
    public Type typeCheck() {
        Type type1 = myExp1.typeCheck();
        Type type2 = myExp2.typeCheck();
        Type retType = new BoolType();
        
        if (!type1.isErrorType() && !type1.isIntType()) {
            ErrMsg.fatal(myExp1.lineNum(), myExp1.charNum(),
                         "Relational operator applied to non-numeric operand");
            retType = new ErrorType();
        }
        
        if (!type2.isErrorType() && !type2.isIntType()) {
            ErrMsg.fatal(myExp2.lineNum(), myExp2.charNum(),
                         "Relational operator applied to non-numeric operand");
            retType = new ErrorType();
        }
        
        if (type1.isErrorType() || type2.isErrorType()) {
            retType = new ErrorType();
        }
        
        return retType;
    }
}

class PlusNode extends ArithmeticExpNode {
    public PlusNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
        myOp = "add";
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" + ");
        myExp2.unparse(p, 0);
        p.print(")");
    }
}

class MinusNode extends ArithmeticExpNode {
    public MinusNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
        myOp = "sub";
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" - ");
        myExp2.unparse(p, 0);
        p.print(")");
    }
}

class TimesNode extends ArithmeticExpNode {
    public TimesNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
        myOp = "mulo";
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" * ");
        myExp2.unparse(p, 0);
        p.print(")");
    }
}

class DivideNode extends ArithmeticExpNode {
    public DivideNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
        myOp = "div";
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" / ");
        myExp2.unparse(p, 0);
        p.print(")");
    }
}

class AndNode extends LogicalExpNode {
    public AndNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
        myOp = "0";
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" && ");
        myExp2.unparse(p, 0);
        p.print(")");
    }
}

class OrNode extends LogicalExpNode {
    public OrNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
        myOp = "1";
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" || ");
        myExp2.unparse(p, 0);
        p.print(")");
    }
}

class EqualsNode extends EqualityExpNode {
    public EqualsNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
        myOp = "beq";
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" == ");
        myExp2.unparse(p, 0);
        p.print(")");
    }
}

class NotEqualsNode extends EqualityExpNode {
    public NotEqualsNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
        myOp = "bne";
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" != ");
        myExp2.unparse(p, 0);
        p.print(")");
    }
}

class LessNode extends RelationalExpNode {
    public LessNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
        myOp = "bltz";
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" < ");
        myExp2.unparse(p, 0);
        p.print(")");
    }
}

class GreaterNode extends RelationalExpNode {
    public GreaterNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
        myOp = "bgtz";
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" > ");
        myExp2.unparse(p, 0);
        p.print(")");
    }
}

class LessEqNode extends RelationalExpNode {
    public LessEqNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
        myOp = "blez";
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" <= ");
        myExp2.unparse(p, 0);
        p.print(")");
    }
}

class GreaterEqNode extends RelationalExpNode {
    public GreaterEqNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
        myOp = "bgez";
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" >= ");
        myExp2.unparse(p, 0);
        p.print(")");
    }
}
