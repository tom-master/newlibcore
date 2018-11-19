using System;

namespace NewLibCore.Data.Mongodb.Bson.Serialization.IdGenerators
{
    public class StringGiudGenerator : IIdGenerator
    {
        private static StringGiudGenerator __instance = new StringGiudGenerator();


        public StringGiudGenerator() { }

        public static StringGiudGenerator Instance
        {
            get { return __instance; }
        }

        public object GenerateId ( object container , object document )
        {
            return Guid.NewGuid().ToString();
        }

        public bool IsEmpty ( object id )
        {
            return (id + "").Length == 0;
        }
    }
}
