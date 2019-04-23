import wikipedia
import json
import nltk

# nltk.download()
topics = [
    "David Gries",
    "Alan Turing",
    "Algorithm",
    "Anita Borg",
    "Apple Inc.",
    "Artificial Intelligence",
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
    "Eva Tardos",
    "Jon Kleinberg",
    "David Easley",
    "Robert Kleinberg",
    "John Hopcroft",
    "Central processing unit",
    "Cornell University",
    "Stanford University",
    "Harvard University",
    "Massachusetts Institute of Technology",
    "California Institute of Technology",
    "Silicon Valley",
    "Venture Capital",
    "University of California, Berkeley",
    "Carnegie Mellon University",
    "Georgia Institute of Technology",
    "NASDAQ",
    "Robinhood",
    "Netflix",
    "PayPal",
    "Steve Wozniak",
    "LinkedIn",
    "Mozilla",
    "Quora",
    "Palo Alto",
    "Brian Acton",
    "Steve Ballmer",
    "Instagram",
    "Kevin Systrom",
    "Mike Krieger",
    "Karlie Kloss",
    "Andreessen Horowitz",
    "Data structure",
    "Database",
    "Python (programming language)",
    "OCaml",
    "JavaScript",
    "HTML",
    "CSS",
    "Application programming interface",
    "Windows",
    "macOS",
    "Operating system",
    "Compiler",
    "Bytecode",
    "LLVM",
    "C++",
    "Object-oriented programming",
    "Metaprogramming",
    "Functional programming",
    "Database",
    "Cloud computing",
    "Amazon Web Services",
    "Internet",
    "IP address",
    "Concurrency (computer science)",
    "Edsger W. Dijkstra",
    "Software engineer",
    "Open-source software",
    "Copyright",
    "Microservices",
    "Moore's law",
    "Nvidia",
    "Advanced Micro Devices",
    "Qualcomm",
    "C (programming language)",
    "Dennis Ritchie",
    "Physics",
    "Albert Einstein",
    "Grace Hopper",
    "Women in computing",
    "NASA"
    ]

# data = {}
d = []
vocab_size = set()
vocab_size_2 = set()
for topic in topics:
    content = wikipedia.page(topic).content
    sent_tokenized_content = nltk.sent_tokenize(content)
    final_content = []
    for sentence in sent_tokenized_content:
        if len(sentence) > 3:
            final_content.append(sentence)
    # article = {}
    # article["topic"] = topic
    # article["content"] = final_content
    vocab_size.update(set(sent_tokenized_content))
    vocab_size_2.update(set(final_content))
    # d.append(article)

    # if topic in data:
    #     data[topic]["content"] = sent_tokenized_content
    # else:
    #     data[topic] = {}
    #     data[topic]["content"] = sent_tokenized_content

    # file_name = topic + ".txt"
    # text_file = open(file_name, "w")
    #
    # text_file.write(content)
    #
    # text_file.close()
print("vocab size 1:")
print(len(vocab_size))
print("vocab size 2:")
print(len(vocab_size_2))
# with open("improved_data.json", "w") as outfile:
#     json.dump(d, outfile, ensure_ascii=False)
