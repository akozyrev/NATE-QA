import wikipedia

topics = ["Mathematics"]
for topic in topics:
  stuff = wikipedia.page(topic).content

  file_name = topic + ".txt"
  text_file = open(file_name, "w")

  text_file.write(stuff)

  text_file.close()
