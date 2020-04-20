# oops-lang

Testing the Parser:

    1. $ ocamlbuild test1.native
    2. $ ./test1.native
    3. Enter the sample input
    4. Ctrl D 

    Sample Input:

    public class Test{
        int a;
        int test_func(int b){
            a = b;
        }
    }

Testing the Semantics:

    1. $ocamlbuild test2.native
    2. $./test2.native
    3. Enter the sample intput
    4. Ctrl D

    Sample Input:

    // this causes a duplicate class name error

    public class Test{
        int test(){
            return 4;
        }
    }
    public class Test{
        int test(){
            return 5;
        }
    }
    int main(){
        Test t1;
        return 0;
    }

TO-DO:
    Scanner/Parser: arrays, recursive method calls (e.g. object.foo().foo().foo();), 
    Semantics: check that object types match a declared class name, local and global variable scopes for classes ... // add more