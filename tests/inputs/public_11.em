class Super < Object
begin
  def foo()
    @x=1;
	a=@y.to_s();
	@y=2;
	b=a.+(@y.to_s())
  end
end

(new Super).foo()
