using System;
using System.Collections.Generic;
using System.Data.Common;
using System.IO;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;
using System.Text.RegularExpressions;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using NewLibCore.Data.Mapper.DataExtension;
using NewLibCore.Data.Mapper.InternalDataStore;
using NewLibCore.Data.Mapper.MapperExtension;
using NewLibCore.Data.Mapper.PropertyExtension;


namespace UnitTestProject1
{
	[TestClass]
	public class UnitTest1
	{
		[TestMethod]
		public void TestMethod1()
		{
			NodeList nodes = new NodeList(0);
			nodes.Append(1);
			nodes.Append(2);
			nodes.Append(3);
			nodes.Append(4);
			nodes.Append(5);
			nodes.Append(6);
			nodes.Append(7);
			nodes.Append(8);
			nodes.Append(9);
			var result = nodes;

			var r = nodes.GetMiddleValue();
		}
	}

	public class NodeList
	{
		private NodeList _next;
		private Int32 _data;

		public NodeList(Int32 data)
		{
			_next = null;
			_data = data;
		}

		public void Append(Int32 data)
		{
			var newNode = new NodeList(data);
			var root = this;
			if (root._next == null)
			{
				root._next = newNode;
				return;
			}
			while (root._next != null)
			{
				root = root._next;
			}
			root._next = newNode;
		}

		public Int32 GetMiddleValue()
		{
			var fast = this;
			var slow = this;

			while (fast != null && fast._next != null)
			{
				fast = fast._next._next;
				slow = slow._next;
			}

			return slow._data;
		}
	}
}
