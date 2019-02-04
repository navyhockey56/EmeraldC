class Test < Object
begin
  def test(a)
      a = 12;
      a
  end
end

(new Test).test(21)
