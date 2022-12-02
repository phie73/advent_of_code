import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class a01
{

    private static final String file = "input02.txt";
    private static final Map<String, Integer> scores = new HashMap<>();
    private static final Map<String, Integer> scores2 = new HashMap<>();

    static
    {
        scores.put("A X", 4); //A,X -> 1 B,Y -> 2, C,Z -> 3
        scores.put("A Y", 8);
        scores.put("A Z", 3);
        scores.put("B X", 1);
        scores.put("B Y", 5);
        scores.put("B Z", 9);
        scores.put("C X", 7);
        scores.put("C Y", 2);
        scores.put("C Z", 6);    

        scores2.put("A X", 3); //X loos, Y draw, Z win
        scores2.put("A Y", 4);
        scores2.put("A Z", 8);
        scores2.put("B X", 1);
        scores2.put("B Y", 5);
        scores2.put("B Z", 9);
        scores2.put("C X", 2);
        scores2.put("C Y", 6);
        scores2.put("C Z", 7);
    }

    private static String parseFile(String fileName) throws IOException
    {
        return new String(Files.readAllBytes(Paths.get(fileName)));
    }

    private static List<String> readFile() throws IOException
    {
        return Arrays.asList(parseFile(file).split("\n"));
    }

    public static void solve() throws IOException 
    {
        List<String> dataList = readFile();
        int score = dataList.stream().mapToInt(x->scores.get(x)).sum();
        int score2 = dataList.stream().mapToInt(x->scores2.get(x)).sum();
        System.out.println("my score: " + score);
        System.out.println("my score2: " + score2);
    }

    public static void main(String[] args)
    {
        try
        {
            a01.solve();
        } catch (IOException e)
        {
            e.printStackTrace();
        }
    }

}