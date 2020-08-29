using System;
using NpgsqlFSharpParser;

namespace ParserTestsWithNet48
{
    static class Program
    {
        static void Main(string[] args)
        {
            // Making sure the code from the parser
            // can be called from a net48 .NET Framework library
            // simulating the VS extension
            var query = "SELECT * FROM users";
            var result = Parser.parseUnsafe(query);
            Console.WriteLine(result.ToString());
        }
    }
}
