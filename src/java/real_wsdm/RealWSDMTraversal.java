package real_wsdm;

import org.lemurproject.galago.core.index.stats.AggregateStatistic;
import org.lemurproject.galago.core.index.stats.NodeStatistics;
import org.lemurproject.galago.core.parse.stem.KrovetzStemmer;
import org.lemurproject.galago.core.parse.stem.Stemmer;
import org.lemurproject.galago.core.retrieval.GroupRetrieval;
import org.lemurproject.galago.core.retrieval.Retrieval;
import org.lemurproject.galago.core.retrieval.query.MalformedQueryException;
import org.lemurproject.galago.core.retrieval.query.Node;
import org.lemurproject.galago.core.retrieval.query.NodeParameters;
import org.lemurproject.galago.core.retrieval.traversal.Traversal;
import org.lemurproject.galago.core.util.TextPartAssigner;
import org.lemurproject.galago.utility.Parameters;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;
import java.util.logging.Logger;
import java.util.stream.Collectors;

/**
 * Weighted Sequential Dependency Model model is structurally similar to the
 * Sequential Dependency Model, however node weights are the linear combination
 * of some node features.
 * <p>
 * (based on bendersky 2012)
 * <p>
 * In particular the weight for a node "term" is determined as a linear
 * combination of features:
 * <p>
 * Feature def for RWSDM: <br>
 * { <br>
 * name : "1-gram" <br>
 * type : [ const, logtf, logdf ] -- (tf default) <br>
 * group : "retrievalGroupName" :: missing or empty = default <br>
 * part : "retrievalPartName" :: missing or empty = default <br>
 * unigram : true|false :: can be used on unigrams <br>
 * bigram : true|false :: can be used on bigrams <br>
 * } <br>
 *
 * @author sjh, fsqcds
 */
public class RealWSDMTraversal extends Traversal {
    private static final Logger logger = Logger.getLogger("RWSDM");
    private static Stemmer stemmer = new KrovetzStemmer();
    private Retrieval retrieval;
    private GroupRetrieval gRetrieval;
    private Parameters globalParams;
    private boolean defCombNorm;
    private boolean verbose;
    private List<RWSDMFeature> uniFeatures;
    private List<RWSDMFeature> biFeatures;
    private List<RWSDMFeature> triFeatures;

    public RealWSDMTraversal(Retrieval retrieval) throws Exception {
        if (retrieval instanceof GroupRetrieval) {
            gRetrieval = (GroupRetrieval) retrieval;
        }
        this.retrieval = retrieval;

        this.globalParams = retrieval.getGlobalParameters();

        verbose = globalParams.get("verboseRWSDM", false);
        defCombNorm = globalParams.get("norm", false);

        uniFeatures = new ArrayList();
        biFeatures = new ArrayList();
        triFeatures = new ArrayList();

        if (globalParams.isList("rwsdmFeatures", Parameters.class)) {
            for (Parameters f : (List<Parameters>) globalParams.getList("rwsdmFeatures")) {
                RWSDMFeature wf = new RWSDMFeature(f);
                if (wf.unigram) {
                    uniFeatures.add(wf);
                }
                if (wf.bigram) {
                    biFeatures.add(wf);
                }
                if (wf.trigram) {
                    triFeatures.add(wf);
                }
            }

        } else {
            // default list of features: (using target collection only)
            uniFeatures.add(new RWSDMFeature("1-const", RWSDMFeatureType.CONST, 0.8, true));
            uniFeatures.add(new RWSDMFeature("1-lntf", RWSDMFeatureType.LOGTF, 0.0, true));
            uniFeatures.add(new RWSDMFeature("1-lndf", RWSDMFeatureType.LOGDF, 0.0, true));

            biFeatures.add(new RWSDMFeature("2-const", RWSDMFeatureType.CONST, 0.1, false));
            biFeatures.add(new RWSDMFeature("2-lntf", RWSDMFeatureType.LOGTF, 0.0, false));
            biFeatures.add(new RWSDMFeature("2-lndf", RWSDMFeatureType.LOGDF, 0.0, false));
        }
    }

    @Override
    public void beforeNode(Node original, Parameters queryParameters) throws Exception {
    }

    @Override
    public Node afterNode(Node original, Parameters queryParams) throws Exception {
        if (original.getOperator().equals("rwsdm")) {

            NodeParameters np = original.getNodeParameters();

            // First check format - should only contain text node children
            List<Node> children = original.getInternalNodes();
            for (Node child : children) {
                if (child.getOperator().equals("text") == false) {
                    throw new MalformedQueryException("wsdm operator requires text-only children");
                }
            }

            // formatting is ok - now reassemble
            ArrayList<Node> newChildren = new ArrayList();
            NodeParameters newWeights = new NodeParameters();
            // i don't want normalization -- even though michael used some.
            newWeights.set("norm", defCombNorm);


            for (Node child : children) {
                String term = child.getDefaultParameter();

                double weight = computeWeight(term, np, queryParams);
                newWeights.set(Integer.toString(newChildren.size()), weight);
                newChildren.add(child.clone());
            }

            if (!biFeatures.isEmpty()) {
                for (int i = 0; i < (children.size() - 1); i++) {
                    ArrayList<Node> pair = new ArrayList();
                    pair.add(new Node("extents", children.get(i).getDefaultParameter()));
                    pair.add(new Node("extents", children.get(i + 1).getDefaultParameter()));

                    double weight = computeWeight(pair.get(0).getDefaultParameter(), pair.get(1).getDefaultParameter(), np, queryParams);

                    newWeights.set(Integer.toString(newChildren.size()), weight);
                    newChildren.add(new Node("od", new NodeParameters(1), Node.cloneNodeList(pair)));

                    newWeights.set(Integer.toString(newChildren.size()), weight);
                    newChildren.add(new Node("uw", new NodeParameters(8), Node.cloneNodeList(pair)));
                }
            }

            if (!triFeatures.isEmpty()) {
                for (int i = 0; i < (children.size() - 2); i++) {
                    ArrayList<Node> triple = new ArrayList();
                    triple.add(new Node("extents", children.get(i).getDefaultParameter()));
                    triple.add(new Node("extents", children.get(i + 1).getDefaultParameter()));
                    triple.add(new Node("extents", children.get(i + 2).getDefaultParameter()));

                    double weight = computeWeight(triple.get(0).getDefaultParameter(), triple.get(1).getDefaultParameter(), triple.get(2).getDefaultParameter(), np, queryParams);

                    newWeights.set(Integer.toString(newChildren.size()), weight);
                    newChildren.add(new Node("od", new NodeParameters(1), Node.cloneNodeList(triple)));

                    newWeights.set(Integer.toString(newChildren.size()), weight);
                    newChildren.add(new Node("uw", new NodeParameters(12), Node.cloneNodeList(triple)));
                }
            }

            Node wsdm = new Node("combine", newWeights, newChildren, original.getPosition());

            if (verbose) {
                System.err.println(wsdm.toPrettyString());
            }

            return wsdm;
        } else {
            return original;
        }
    }

    private double computeWeight(String term, NodeParameters np, Parameters queryParams) throws Exception {

        // we will probably need this for several features :
        Node t = new Node("counts", term);
        t = TextPartAssigner.assignPart(t, queryParams, retrieval.getAvailableParts());

        // feature value store
        Map<RWSDMFeature, Double> featureValues = new HashMap();

        // tf/df comes from the same object - can be used  twice
        Map<String, AggregateStatistic> localCache = new HashMap();

        // NOW : collect some feature values
        Node node;
        NodeStatistics featureStats;
        String cacheString;

        for (RWSDMFeature f : uniFeatures) {
            switch (f.type) {
                case CONST:
                    assert (!featureValues.containsKey(f));
                    featureValues.put(f, 1.0);
                    break;

                case LOGTF:
                case LOGNGRAMTF: // unigrams are the same
                    assert (!featureValues.containsKey(f));

                    // if the feature weight is 0 -- don't compute the feature
                    if (queryParams.get(f.name, f.defLambda) == 0.0) {
                        break;
                    }

                    node = t;
                    if (!f.part.isEmpty()) {
                        node = t.clone();
                        node.getNodeParameters().set("part", f.part);
                    }
                    cacheString = node.toString() + "-" + f.group;

                    if (localCache.containsKey(cacheString)) {
                        featureStats = (NodeStatistics) localCache.get(cacheString);
                    } else if (gRetrieval != null && !f.group.isEmpty()) {
                        featureStats = gRetrieval.getNodeStatistics(node, f.group);
                        localCache.put(cacheString, featureStats);
                    } else {
                        featureStats = this.retrieval.getNodeStatistics(node);
                        localCache.put(cacheString, featureStats);
                    }

                    // only add the value if it occurs in the collection (log (0) = -Inf)
                    if (featureStats.nodeFrequency != 0) {
                        featureValues.put(f, Math.log(featureStats.nodeFrequency));
                    }

                    break;

                case LOGDF:
                    assert (!featureValues.containsKey(f));
                    // if the feature weight is 0 -- don't compute the feature
                    if (queryParams.get(f.name, f.defLambda) == 0.0) {
                        break;
                    }

                    node = t;
                    if (!f.part.isEmpty()) {
                        node = t.clone();
                        node.getNodeParameters().set("part", f.part);
                    }
                    cacheString = node.toString() + "-" + f.group;

                    if (localCache.containsKey(cacheString)) {
                        featureStats = (NodeStatistics) localCache.get(cacheString);
                    } else if (gRetrieval != null && !f.group.isEmpty()) {
                        featureStats = gRetrieval.getNodeStatistics(node, f.group);
                        localCache.put(cacheString, featureStats);
                    } else {
                        featureStats = this.retrieval.getNodeStatistics(node);
                        localCache.put(cacheString, featureStats);
                    }

                    // only add the value if it occurs in the collection (log (0) = -Inf)
                    if (featureStats.nodeDocumentCount != 0) {
                        featureValues.put(f, Math.log(featureStats.nodeDocumentCount));
                    }

                    break;

                case EXTERNAL:
                    assert (!featureValues.containsKey(f));

                    // if the feature weight is 0 -- don't compute the feature
                    if (queryParams.get(f.name, f.defLambda) == 0.0) {
                        break;
                    }

                    if (f.containsNGram(term)) {
                        featureValues.put(f, Math.log(f.getNGramValue(term)));
                    }

                    break;
            }
        }

        double weight = 0.0;
        for (RWSDMFeature f : uniFeatures) {
            double lambda = np.get(f.name, queryParams.get(f.name, f.defLambda));
            if (featureValues.containsKey(f)) {
                weight += lambda * featureValues.get(f);
                if (verbose) {
                    logger.info(String.format("%s -- feature:%s:%g * %g = %g", term, f.name, lambda, featureValues.get(f), lambda * featureValues.get(f)));
                }
            }
        }

        return weight;
    }

    private double computeWeight(String term1, String term2, NodeParameters np, Parameters queryParams) throws Exception {

        // prepare nodes (will be used several times)
        Node t1 = new Node("extents", term1);
        t1 = TextPartAssigner.assignPart(t1, queryParams, retrieval.getAvailableParts());
        Node t2 = new Node("extents", term2);
        t2 = TextPartAssigner.assignPart(t2, queryParams, retrieval.getAvailableParts());

        Node od1 = new Node("ordered");
        od1.getNodeParameters().set("default", 1);
        od1.addChild(t1);
        od1.addChild(t2);

        // feature value store
        Map<RWSDMFeature, Double> featureValues = new HashMap();

        // tf/df comes from the same object - can be used  twice
        Map<String, AggregateStatistic> localCache = new HashMap();

        // NOW : collect some feature values
        Node node;
        NodeStatistics featureStats;
        String cacheString;

        for (RWSDMFeature f : biFeatures) {
            switch (f.type) {
                case CONST:
                    assert (!featureValues.containsKey(f));
                    featureValues.put(f, 1.0);
                    break;

                case LOGTF:
                    assert (!featureValues.containsKey(f));
                    // if the feature weight is 0 -- don't compute the feature
                    if (queryParams.get(f.name, f.defLambda) == 0.0) {
                        break;
                    }

                    node = od1;
                    if (!f.part.isEmpty()) {
                        node = od1.clone();
                        node.getChild(0).getNodeParameters().set("part", f.part);
                        node.getChild(1).getNodeParameters().set("part", f.part);
                    }
                    // f.group is "" or some particular group
                    cacheString = node.toString() + "-" + f.group;

                    // first check if we have already done this node.
                    if (localCache.containsKey(cacheString)) {
                        featureStats = (NodeStatistics) localCache.get(cacheString);
                    } else if (gRetrieval != null && !f.group.isEmpty()) {
                        featureStats = gRetrieval.getNodeStatistics(node, f.group);
                        localCache.put(cacheString, featureStats);
                    } else {
                        featureStats = this.retrieval.getNodeStatistics(node);
                        localCache.put(cacheString, featureStats);
                    }

                    // only add the value if it occurs in the collection (log (0) = -Inf)
                    if (featureStats.nodeFrequency != 0) {
                        featureValues.put(f, Math.log(featureStats.nodeFrequency));
                    }

                    break;

                case LOGDF:
                    assert (!featureValues.containsKey(f));
                    // if the feature weight is 0 -- don't compute the feature
                    if (queryParams.get(f.name, f.defLambda) == 0.0) {
                        break;
                    }

                    node = od1;
                    if (!f.part.isEmpty()) {
                        node = od1.clone();
                        node.getChild(0).getNodeParameters().set("part", f.part);
                        node.getChild(1).getNodeParameters().set("part", f.part);
                    }
                    cacheString = node.toString() + "-" + f.group;

                    if (localCache.containsKey(cacheString)) {
                        featureStats = (NodeStatistics) localCache.get(cacheString);
                    } else if (gRetrieval != null && !f.group.isEmpty()) {
                        featureStats = gRetrieval.getNodeStatistics(node, f.group);
                        localCache.put(cacheString, featureStats);
                    } else {
                        featureStats = this.retrieval.getNodeStatistics(node);
                        localCache.put(cacheString, featureStats);
                    }

                    // only add the value if it occurs in the collection (log (0) = -Inf)
                    if (featureStats.nodeDocumentCount != 0) {
                        featureValues.put(f, Math.log(featureStats.nodeDocumentCount));
                    }

                    break;

                case LOGNGRAMTF:
                    assert (!featureValues.containsKey(f));
                    // if the feature weight is 0 -- don't compute the feature
                    if (queryParams.get(f.name, f.defLambda) == 0.0) {
                        break;
                    }

                    node = new Node("counts", term1 + "~" + term2);
                    if (!f.part.isEmpty()) {
                        node.getNodeParameters().set("part", f.part);
                    }
                    // f.group is "" or some particular group
                    cacheString = node.toString() + "-" + f.group;

                    // first check if we have already done this node.
                    if (localCache.containsKey(cacheString)) {
                        featureStats = (NodeStatistics) localCache.get(cacheString);
                    } else if (gRetrieval != null && !f.group.isEmpty()) {
                        featureStats = gRetrieval.getNodeStatistics(node, f.group);
                        localCache.put(cacheString, featureStats);
                    } else {
                        featureStats = this.retrieval.getNodeStatistics(node);
                        localCache.put(cacheString, featureStats);
                    }

                    // only add the value if it occurs in the collection (log (0) = -Inf)
                    if (featureStats.nodeFrequency != 0) {
                        featureValues.put(f, Math.log(featureStats.nodeFrequency));
                    }

                    break;

                case EXTERNAL:
                    assert (!featureValues.containsKey(f));

                    // if the feature weight is 0 -- don't compute the feature
                    if (queryParams.get(f.name, f.defLambda) == 0.0) {
                        break;
                    }

                    if (f.containsNGram(term1, term2)) {
                        featureValues.put(f, Math.log(f.getNGramValue(term1, term2)));
                    }

                    break;

            }
        }

        double weight = 0.0;
        for (RWSDMFeature f : biFeatures) {
            double lambda = np.get(f.name, queryParams.get(f.name, f.defLambda));
            if (featureValues.containsKey(f)) {
                weight += lambda * featureValues.get(f);
                if (verbose) {
                    logger.info(String.format("%s, %s -- feature:%s:%g * %g = %g", term1, term2, f.name, lambda, featureValues.get(f), lambda * featureValues.get(f)));
                }
            }
        }

        return weight;
    }

    private double computeWeight(String term1, String term2, String term3, NodeParameters np, Parameters queryParams) throws Exception {

        // prepare nodes (will be used several times)
        Node t1 = new Node("extents", term1);
        t1 = TextPartAssigner.assignPart(t1, queryParams, retrieval.getAvailableParts());
        Node t2 = new Node("extents", term2);
        t2 = TextPartAssigner.assignPart(t2, queryParams, retrieval.getAvailableParts());
        Node t3 = new Node("extents", term3);
        t3 = TextPartAssigner.assignPart(t3, queryParams, retrieval.getAvailableParts());

        Node od1 = new Node("ordered");
        od1.getNodeParameters().set("default", 1);
        od1.addChild(t1);
        od1.addChild(t2);
        od1.addChild(t3);

        // feature value store
        Map<RWSDMFeature, Double> featureValues = new HashMap();

        // tf/df comes from the same object - can be used twice
        Map<String, AggregateStatistic> localCache = new HashMap();

        // NOW : collect some feature values
        Node node;
        NodeStatistics featureStats;
        String cacheString;

        for (RWSDMFeature f : triFeatures) {
            switch (f.type) {
                case CONST:
                    assert (!featureValues.containsKey(f));
                    featureValues.put(f, 1.0);
                    break;

                case LOGTF:
                    assert (!featureValues.containsKey(f));
                    // if the feature weight is 0 -- don't compute the feature
                    if (queryParams.get(f.name, f.defLambda) == 0.0) {
                        break;
                    }

                    node = od1;
                    if (!f.part.isEmpty()) {
                        node = od1.clone();
                        node.getChild(0).getNodeParameters().set("part", f.part);
                        node.getChild(1).getNodeParameters().set("part", f.part);
                        node.getChild(2).getNodeParameters().set("part", f.part);
                    }
                    // f.group is "" or some particular group
                    cacheString = node.toString() + "-" + f.group;

                    // first check if we have already done this node.
                    if (localCache.containsKey(cacheString)) {
                        featureStats = (NodeStatistics) localCache.get(cacheString);
                    } else if (gRetrieval != null && !f.group.isEmpty()) {
                        featureStats = gRetrieval.getNodeStatistics(node, f.group);
                        localCache.put(cacheString, featureStats);
                    } else {
                        featureStats = this.retrieval.getNodeStatistics(node);
                        localCache.put(cacheString, featureStats);
                    }

                    // only add the value if it occurs in the collection (log (0) = -Inf)
                    if (featureStats.nodeFrequency != 0) {
                        featureValues.put(f, Math.log(featureStats.nodeFrequency));
                    }

                    break;

                case LOGDF:
                    assert (!featureValues.containsKey(f));
                    // if the feature weight is 0 -- don't compute the feature
                    if (queryParams.get(f.name, f.defLambda) == 0.0) {
                        break;
                    }

                    node = od1;
                    if (!f.part.isEmpty()) {
                        node = od1.clone();
                        node.getChild(0).getNodeParameters().set("part", f.part);
                        node.getChild(1).getNodeParameters().set("part", f.part);
                        node.getChild(2).getNodeParameters().set("part", f.part);
                    }
                    cacheString = node.toString() + "-" + f.group;

                    if (localCache.containsKey(cacheString)) {
                        featureStats = (NodeStatistics) localCache.get(cacheString);
                    } else if (gRetrieval != null && !f.group.isEmpty()) {
                        featureStats = gRetrieval.getNodeStatistics(node, f.group);
                        localCache.put(cacheString, featureStats);
                    } else {
                        featureStats = this.retrieval.getNodeStatistics(node);
                        localCache.put(cacheString, featureStats);
                    }

                    // only add the value if it occurs in the collection (log (0) = -Inf)
                    if (featureStats.nodeDocumentCount != 0) {
                        featureValues.put(f, Math.log(featureStats.nodeDocumentCount));
                    }

                    break;

                case LOGNGRAMTF:
                    assert (!featureValues.containsKey(f));
                    // if the feature weight is 0 -- don't compute the feature
                    if (queryParams.get(f.name, f.defLambda) == 0.0) {
                        break;
                    }

                    node = new Node("counts", term1 + "~" + term2 + "~" + term3);
                    if (!f.part.isEmpty()) {
                        node.getNodeParameters().set("part", f.part);
                    }
                    // f.group is "" or some particular group
                    cacheString = node.toString() + "-" + f.group;

                    // first check if we have already done this node.
                    if (localCache.containsKey(cacheString)) {
                        featureStats = (NodeStatistics) localCache.get(cacheString);
                    } else if (gRetrieval != null && !f.group.isEmpty()) {
                        featureStats = gRetrieval.getNodeStatistics(node, f.group);
                        localCache.put(cacheString, featureStats);
                    } else {
                        featureStats = this.retrieval.getNodeStatistics(node);
                        localCache.put(cacheString, featureStats);
                    }

                    // only add the value if it occurs in the collection (log (0) = -Inf)
                    if (featureStats.nodeFrequency != 0) {
                        featureValues.put(f, Math.log(featureStats.nodeFrequency));
                    }

                    break;

                case EXTERNAL:
                    assert (!featureValues.containsKey(f));

                    // if the feature weight is 0 -- don't compute the feature
                    if (queryParams.get(f.name, f.defLambda) == 0.0) {
                        break;
                    }

                    if (f.containsNGram(term1, term2, term3)) {
                        featureValues.put(f, Math.log(f.getNGramValue(term1, term2, term3)));
                    }

                    break;
            }
        }

        double weight = 0.0;
        for (RWSDMFeature f : triFeatures) {
            double lambda = np.get(f.name, queryParams.get(f.name, f.defLambda));
            if (featureValues.containsKey(f)) {
                weight += lambda * featureValues.get(f);
                if (verbose) {
                    logger.info(String.format("%s, %s, %s -- feature:%s:%g * %g = %g", term1, term2, term3, f.name, lambda, featureValues.get(f), lambda * featureValues.get(f)));
                }
            }
        }

        return weight;
    }

    public static enum RWSDMFeatureType {

        LOGTF, LOGDF, CONST, LOGNGRAMTF, EXTERNAL
    }

    /*
     * Features for WSDM:
     *  name : "1-gram"
     *  tfFeature : [true | false] :: asserts [ tf or df ], (tf default)
     *  group : "retrievalGroupName" :: missing or empty = default
     *  part : "retrievalPartName" :: missing or empty = default
     *  unigram : true|false :: can be used on unigrams
     *  bigram : true|false :: can be used on bigrams
     */
    public static class RWSDMFeature {

        public String name;
        public RWSDMFeatureType type; // [tf | df | const] -- others may be supported later
        public String group;
        public String part;
        public double defLambda;
        // mutually exclusive unigram/bigram
        public boolean unigram;
        public boolean bigram;
        public boolean trigram;
        public Map<List<String>, Integer> featureValues;

        private static Map<String, Map<List<String>, Integer>> valuesCache = new HashMap<>();

        public RWSDMFeature(Parameters p) {
            this.name = p.getString("name");
            this.type = RWSDMFeatureType.valueOf(p.get("type", "logtf").toUpperCase());
            this.defLambda = p.get("lambda", 1.0);
            this.group = p.get("group", "");
            this.part = p.get("part", "");
            this.unigram = p.get("unigram", true);
            this.bigram = p.get("bigram", !unigram);
            this.trigram = p.get("trigram", !unigram && !bigram);
            if (this.type.equals(RWSDMFeatureType.EXTERNAL)) {
                String path = p.getString("path");
                if (valuesCache.containsKey(path)) {
                    this.featureValues = valuesCache.get(path);
                } else {
                    this.featureValues = readFeatureValues(p.getString("path"));
                    valuesCache.put(path, this.featureValues);
                }
            }
        }

        /*
         * Constructor to allow default list of features
         */
        public RWSDMFeature(String name, RWSDMFeatureType type, double defLambda, boolean unigram) {
            this.name = name;
            this.type = type;
            this.defLambda = defLambda;
            this.group = "";
            this.part = "";
            this.unigram = unigram;
            this.bigram = !unigram;
            this.trigram = !unigram;
        }

        private List<String> stemNGram(List<String> grams) {
            return grams.stream().map(stemmer::stem).collect(Collectors.toList());
        }

        public boolean containsNGram(String... grams) {
            return featureValues.containsKey(stemNGram(Arrays.asList(grams)));
        }

        public Integer getNGramValue(String... grams) {
            return featureValues.get(stemNGram(Arrays.asList(grams)));
        }

        private Map<List<String>, Integer> readFeatureValues(String path) {
            Map<List<String>, Integer> values = new HashMap<>();
            logger.info(String.format("Start reading values from %s", path));
            try (BufferedReader reader = Files.newBufferedReader(Paths.get(path))) {
                String line;
                while ((line = reader.readLine()) != null) {
                    String[] parts = line.split("\\t");
                    String[] grams = parts[0].split(" ");
                    values.put(Arrays.asList(grams), Integer.parseInt(parts[1]));
                }
            } catch (IOException x) {
                System.err.format("IOException: %s%n", x);
            }
            logger.info(String.format("Finished reading values from %s", path));
            return values;
        }
    }
}
