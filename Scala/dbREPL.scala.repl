import java.sql._

Class.forName("org.sqlite.JDBC")
val c = DriverManager.getConnection(
	"""jdbc:sqlite:C:\Users\Karl\Documents\Programming\Android\BarKeep\database\barkeep""")

val CREATE_TABLE = """CREATE TABLE Types (_id INTEGER PRIMARY KEY AUTOINCREMENT, product_type VARCHAR(250), image_path VARCHAR(250))"""
c.executeUpdate(CREATE_TABLE)

val i1 = """INSERT INTO Types VALUES(1,'Whisky',"cocktail_glass.jpg")"""
val i2 = """INSERT INTO Types VALUES(2,'Gin',"cocktail_glass.jpg")"""
val i3 = """INSERT INTO Types VALUES(3,'Vodka',"cocktail_glass.jpg")"""
val i4 = """INSERT INTO Types VALUES(4,'Tequila',"cocktail_glass.jpg")"""
val i5 = """INSERT INTO Types VALUES(5,'Rum',"cocktail_glass.jpg")"""
val i6 = """INSERT INTO Types VALUES(6,'Brandy',"cocktail_glass.jpg")"""
val i7 = """INSERT INTO Types VALUES(7,'Other',"cocktail_glass.jpg")"""


val INSERT_PICTURE = """insert into types(image) values (?)"""

val ps = c.prepareStatement(INSERT_PICTURE);
ps.setBinaryStream(1, fis, fi.length.toInt);
ps.executeUpdate();
c.commit();
ps.close();
fis.close();
c.close();