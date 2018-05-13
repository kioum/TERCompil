class A1{
    int truc;
    void f(){
        //System.out.print("Fonction f");
    }
}

class A2 extends A1{
    void f2(){
        //System.out.print("Fonction f");
    }
}

public class p1 {
    public static void main(String args []){
        int i = 4%2;
        if(i == 0){
            System.out.print("i = 0");
        }
        else {
            i=0;
        }

        System.out.print(i);
        boolean b = false;
        for(i = 0; i <5; i++){
            if(i > 4){
              b = !b;
            }
        }
        int j = 1;
        A1 a = new A2();
        // A1.f();
        System.out.print(b);
        System.out.print(i);
    }
}
