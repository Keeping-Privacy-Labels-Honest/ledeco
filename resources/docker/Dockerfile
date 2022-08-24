from ipa as ledeco

WORKDIR /
RUN git clone https://github.com/Keeping-Privacy-Labels-Honest/ledeco.git
RUN git clone https://github.com/simkoc/scala-stringtablecreator.git
RUN git clone https://github.com/simkoc/prettyPictureMaker.git
WORKDIR /scala-stringtablecreator
RUN sbt publishLocal
WORKDIR /prettyPictureMaker
RUN sbt publishLocal
WORKDIR /ledeco
RUN sbt stage
