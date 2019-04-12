import wikipedia

topics = ["Mathematics", "David Gries", "Artificial Intelligence",
"Computer Vision", "Natural Language Processing", "Deep Learning", "Mark Zuckerberg",
"YouTube", "Apple", "IPhone", "Silicon Valley", "Spotify", "Pinterest",
"Slack Technologies", "Scott Belsky", "Uber", "Warby Parker", "Cornell University",
"Stanford University", "Turing Award", "Intel", "Deepmind", "Reinforcement Learning",
"Steve Jobs", "Venture Capital", "IPad", "Algorithm", "Privacy",
"Jeff Bezos", "Bill Gates", "Elon Musk", "Tesla", "SpaceX", "Tracy Chou",
"Sheryl Sandberg", "Grace Hopper", "Anita Borg", "Barbara Liskov"]
for topic in topics:
  stuff = wikipedia.page(topic).content

  file_name = topic + ".txt"
  text_file = open(file_name, "w")

  text_file.write(stuff)

  text_file.close()
