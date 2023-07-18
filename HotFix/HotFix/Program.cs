namespace HotFix;

public class Program
{
    public static void Main(string[] args)
    {
        TestHotFix testHotFix = new TestHotFix();
        int num = testHotFix.Add(1, 2);
        int num2 = testHotFix.Sub(1, 2);
        System.Console.WriteLine(num);
        System.Console.WriteLine(num2);
        System.Console.ReadKey();
    }
}