class Assert < Object begin
    def init()
        @counter = 1
    end

    def true(anything)
        if anything.equal?(nil) then 
            "Assert true failed: Test number ".print();
            @counter.print();
            "...Quitting\n\n".print();
            x 
        else 
            @counter = @counter.+(1);
            1
        end
    end

    def equal?(a,b)
        if a.equal?(b) then 
            @counter = @counter.+(1);
            1 
        else 
            "Assert equal failed: Test number ".print();
            @counter.print();
            "...Quitting\n\n".print();
            x 
        end
    end

    def false(anything)
        if anything.equal?(nil) then 
            @counter = @counter.+(1);
            1
        else 
            "Assert false failed: Test number ".print();
            @counter.print();
            "...Quitting\n\n".print();
            x 
        end
    end

    def not_equal?(a,b)
        if a.equal?(b) then 
            "Assert equal failed: Test number ".print();
            @counter.print();
            "...Quitting\n\n".print();
            x 
        else
            @counter = @counter.+(1);
            1 
        end
    end
end

class MapCall < Object begin
    def call(k,v) 
        k.print();
        " -> ".print();
        v.print();
        "\n".print()
    end
end

class A < Object begin

    def init(a,b,c)
        assert = new Assert;
        assert.init();
        assert.true(a instanceof Integer);
        assert.true(b instanceof Integer);
        assert.true(c instanceof String);
        @a = a;
        @b = b;
        @c = c
    end

    def do_a()
        @a
    end

    def do_b()
        @b
    end

    def do_c()
        @c
    end

end

class B < A begin
    def do_b()
        "do @b factorial";
        acc = 1;
        b = @b;
        while b do
            acc = acc.*(b);
            b = b.-(1);
            if b.equal?(0) then b = nil else 0 end
        end;
        acc
    end
end


class C < A begin
    def do_c()
        if @d.equal?(nil) then
            @c.+(" world")
        else
            @c.+(" universe")
        end
    end
end

class Z < B begin
    def to_s()
        "a : ".+(self.do_a().to_s().+(", b : ".+(self.do_b().to_s().+(", c : ".+(self.do_c().to_s())))))
    end
end

class D < Z begin
    def do_a()
        @a.*(10)
    end
end

a = new Assert;
a.init();
"test integer methods";
i = 0;
j = new Integer;
a.equal?(i,j);
i = 1;
a.equal?(i.+(1),2);
a.equal?(i.-(1),0);
a.equal?(i.*(50),50);
a.equal?(10./(2),5);
a.equal?(1.to_s(),"1");

"test string methods";

s = "";
t = new String;
a.equal?(s,t);
a.equal?("hello".length(),5);
a.equal?("hello ".+("world"),"hello world");

"test map methods";
m = new Map;
i = 0;
j = 1;
k = "hello";
m.insert(i,"zero");
m.insert(j,"one");
m.insert(k,"world");
a.true(m.has(i));
a.true(m.has(j));
a.true(m.has(k));
a.equal?(m.find(i),"zero");
a.equal?(m.find(j),"one");
a.equal?(m.find(k),"world");
n = new MapCall;
m.iter(n);

a.equal?(nil,nil);

"test objects and inheritance";
clsA = new A;
clsA.init(2,4,"hello");
a.equal?(clsA.do_a(),2);
a.equal?(clsA.do_b(),4);
a.equal?(clsA.do_c(),"hello");
a.equal?(clsA.to_s(),"Object");

clsB = new B;
clsB.init(2,4,"hello");
a.equal?(clsB.do_a(),2);
a.equal?(clsB.do_b(),24);
a.equal?(clsB.do_c(),"hello");
a.equal?(clsB.to_s(),"Object");

clsC = new C;
clsC.init(2,4,"hello");
a.equal?(clsC.do_a(),2);
a.equal?(clsC.do_b(),4);
a.equal?(clsC.do_c(),"hello world");
a.equal?(clsC.to_s(),"Object");

clsZ = new Z;
clsZ.init(2,4,"hello");
a.equal?(clsZ.do_a(),2);
a.equal?(clsZ.do_b(),24);
a.equal?(clsZ.do_c(),"hello");
a.equal?(clsZ.to_s(),"a : 2, b : 24, c : hello");

clsD = new D;
clsD.init(2,4,"hello");
a.equal?(clsD.do_a(),20);
a.equal?(clsD.do_b(),24);
a.equal?(clsD.do_c(),"hello");
a.equal?(clsD.to_s(),"a : 20, b : 24, c : hello");

"make sure that toplevel is an Object";
a.equal?(self.to_s(), "Object")

