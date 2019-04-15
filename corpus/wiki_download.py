import wikipedia
import json
import nltk

# nltk.download()
topics = [
    "David Gries",
    "Alan Turing",
    "Algorithm",
    "Anita Borg",
    "Apple",
    "Artifical Intelligence",
    "Barbara Liskov",
    "Bill Gates",
    "Computer Graphics",
    "Computer Science",
    "Computer Vision",
    "Cornell University",
    "David Gries",
    "Deep Learning",
    "Deepmind",
    "Distributed Computing",
    "Elon Musk",
    "Embedded Systems",
    "Facebook",
    "Grace Hopper",
    "Human Computer Interaction",
    "Intel",
    "iPad",
    "iPhone",
    "Jeff Bezos",
    "Logic",
    "Machine Learning",
    "Mark Zuckerberg",
    "Mathematics",
    "Microsoft",
    "Natural Language Processing",
    "Pinterest",
    "Privacy",
    "Programming Languages",
    "Reinforcement Learning",
    "Scott Belsky",
    "Sheryl Sandberg",
    "Silicon Valley",
    "Slack Technologies",
    "Steve Jobs",
    "Tesla",
    "Tracy Chou",
    "Turing Award",
    "Twitter",
    "Uber",
    "Venture Capital",
    "Warby Parker",
    "Amazon Company",
    "youTube",
]

data = {}
for topic in topics:
    content = wikipedia.page(topic).content
    sent_tokenized_content = nltk.sent_tokenize(content)

    if topic in data:
        data[topic]["content"] = sent_tokenized_content
    else:
        data[topic] = {}
        data[topic]["content"] = sent_tokenized_content

    file_name = topic + ".txt"
    text_file = open(file_name, "w")

    text_file.write(content)

    text_file.close()

with open("data.json", "w") as outfile:
    json.dump(data, outfile, ensure_ascii=False)
