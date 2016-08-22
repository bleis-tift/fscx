﻿//////////////////////////////////////////////////////////////////////////////
// 
// fscx - Expandable F# compiler project
//   Author: Kouji Matsui (@kekyo2), bleis-tift (@bleis-tift)
//   GutHub: https://github.com/fscx-projects/
//
// Creative Commons Legal Code
// 
// CC0 1.0 Universal
// 
//   CREATIVE COMMONS CORPORATION IS NOT A LAW FIRM AND DOES NOT PROVIDE
//   LEGAL SERVICES.DISTRIBUTION OF THIS DOCUMENT DOES NOT CREATE AN
//   ATTORNEY-CLIENT RELATIONSHIP.CREATIVE COMMONS PROVIDES THIS
//   INFORMATION ON AN "AS-IS" BASIS.CREATIVE COMMONS MAKES NO WARRANTIES
//   REGARDING THE USE OF THIS DOCUMENT OR THE INFORMATION OR WORKS
//   PROVIDED HEREUNDER, AND DISCLAIMS LIABILITY FOR DAMAGES RESULTING FROM
//   THE USE OF THIS DOCUMENT OR THE INFORMATION OR WORKS PROVIDED
//   HEREUNDER.
//
//////////////////////////////////////////////////////////////////////////////

using System;
using System.Diagnostics;
using System.IO;
using System.Linq;

namespace FSharp.Expandable
{
    /// <summary>
    /// Compiler driver for fscx (fscx.exe)
    /// </summary>
    internal static class Program
    {
        /// <summary>
        /// Main entry point.
        /// </summary>
        /// <param name="args">Command line arguments (From FSharp.Expandable.Compiler.Tasks)</param>
        /// <returns>Return value</returns>
        public static int Main(string[] args)
        {
            ///////////////////////////////////////////////////////////////////////
            // Crawl filters

            // TODO: improve detection (idea: parse nuspec?)
            //  Current:
            //   packages --+-- fscx-0.1.*     --+-- build --+-- fscx.exe
            //              +-- HogeFilter-1.0 --+-- lib   --+-- net45 --+-- HogeFilter.dll
            //              +-- HagaFilter-1.0 --+-- lib   --+-- net45 --+-- HagaFilter.dll

            //var exeLocation = GetAssemblyLocation(Assembly.GetExecutingAssembly());
            var exeLocation =
                @"D:\PROJECT\fscx\tests\fscx-enabled\sample-filter\bin\Debug";
            var packagesPath =
                Path.Combine(Path.GetDirectoryName(exeLocation), "..", "..");
            var dllPaths =
                Directory.EnumerateFiles(packagesPath, "*.dll", SearchOption.AllDirectories);
            var visitorPaths =
                dllPaths.FilterVisitors();

            foreach (var path in visitorPaths)
            {
                Debug.WriteLine(path);
            }

            ///////////////////////////////////////////////////////////////////////
            // Compile

            var arguments = args.ExtractCompilerArguments();
            arguments.VisitorPaths = visitorPaths;

            return Compiler.CompileWithArguments(logEntry =>
               Console.WriteLine(
                   "{0}({1},{2}): {3} {4}: {5}",
                   logEntry.FileName,
                   logEntry.Line,
                   logEntry.Column,
                   logEntry.Type.ToString().ToLowerInvariant(),
                   string.IsNullOrWhiteSpace(logEntry.Code) ? "" : logEntry.Code,
                   logEntry.Message),
               arguments);
        }
    }
}
