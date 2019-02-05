class MapCall < Object begin
    def call(k,v) 
        k.print();
        " -> ".print();
        v.print();
        "\n".print()
    end
end

class Foo < Object begin
	def foo(a,b)
		a.print();
		b.print()
	end
end

m = new Map;
i = 10;
j = "world";
k = "hello";
m.insert(i,"zero");
m.insert(j, 90);
m.insert(k,"world");
n = new MapCall;
m.iter(n)