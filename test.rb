class Test
  TEST = 1

  def test
    @aa = File.find { :test => '1' }
    File::TEST
  end
end
