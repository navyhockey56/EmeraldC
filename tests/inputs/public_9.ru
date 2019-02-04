class Super < Object
begin
  def foo(a)
      a.+(1)
  end
end

class Sub < Super
begin
  def foo(a)
      a.+(2)
  end
end

(new Super).foo(3)
