import wikipedia
import json
import nltk
# nltk.download()
topics = ["David Gries"]

data = {}
for topic in topics:
  content = wikipedia.page(topic).content
  sent_tokenized_content = nltk.sent_tokenize(content)

  if topic in data:
      data[topic]['content'] = sent_tokenized_content
  else:
      data[topic] = {}
      data[topic]['content'] = sent_tokenized_content

  file_name = topic + ".txt"
  text_file = open(file_name, "w")

  text_file.write(content)

  text_file.close()

with open('data.json', 'w') as outfile:
    json.dump(data, outfile, ensure_ascii=False)
