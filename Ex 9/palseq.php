<!DOCTYPE html PUBLIC
          "-//W3C//DTD XHTML 1.0 Transitional//EN"
          "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">

<head>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
    <title>Find the longest palindrome!</title>
    <style type="text/css">
        <!-- body,
        td,
        th {
            font-family: Verdana, Arial, Helvetica, sans-serif;
            font-size: x-large;
            color: #CCCCCC;
        }
        
        body {
            background-color: #333399;
        }
        
        .question {
            color: #FFCC33;
        }
        
        .emph {
            color: #99ee99;
        }
        
        .alert {
            color: #ee77aa;
        }
        
        .right {
            color: #33FF66;
            font-weight: bold;
        }
        
        .wrong {
            color: #FF3366;
            font-weight: bold;
        }
        
        a:link {
            color: #CCFFFF;
        }
        
        a:visited {
            color: #CCFFFF;
        }
        
        input {
            background-color: #eeee66;
            color: #333399;
        }
        
        code {
            font-family: Consolas, "Andale Mono", "Courier New", monospace, sans-serif;
            color: #eeee99;
            font-size: 120%;
        }
        
        span.removed {
            color: #ff9977;
            text-decoration: underline red;
        }
        
        code.block {
            background-color: #66eeee;
            color: #993333;
            overflow-wrap: break-word;
            display: block;
            border: 1px solid black;
            padding: 8px;
            width: 95%;
            line-height: 1em;
            margin-top: 0.25em;
            margin-bottom: 0.25em;
        }
        
        input.box {
            overflow-wrap: break-word;
            font-family: Consolas, "Andale Mono", "Courier New", monospace, sans-serif;
            font-size: 120%;
            color: #333333;
            border: 1px solid black;
            padding: 8px;
        }
        
        input.button {
            font-size: 120%;
            background-color: #99ee99;
            color: #333399;
            border: 1px solid black;
            padding: 8px;
        }
        
        -->
    </style>
</head>

<body>
    <h1>Find the longest palindrome!</h1>

    <p>I'll give you a string of (up to 1000) letters and I need you to do one simple thing:
    </p>
    <p>Find the <span class="emph">least</span> possible number of letters that, if removed from the given string, what remains is a
        <span class="emph">palindrome</span>.
    </p>
    <blockquote>
        <p>For example, given the string:
            <code>bbccaddabaddacaaacdb</code> the correct answer is <span class="emph">5</span>.
        </p>
        <p>If one removes these five underlined letters:
            <code>b<span class="removed">b</span>ccaddabaddac<span class="removed">aaa</span>c<span class="removed">d</span>b</code> then the remaining string:
            <code>bccaddabaddaccb</code> is indeed a palindrome. It is not possible to obtain a palindrome by removing fewer than five letters.
        </p>
    </blockquote>

    <hr />
    <?php
        $max_rounds = 10;
        if(!isset($_COOKIE["wrong"])) {
            setcookie("wrong", 0, time() + (86400 * 30), "/");
            $wrong = 0;
        }
    ?>
    <?php
        if((isset($_COOKIE["round"]) && $_COOKIE["round"] < $max_rounds+1) || (!isset($_COOKIE["round"]))) {
            echo '<p><span class="question">Question <span id="questNo">';
            if(!isset($_COOKIE["round"])) {
                setcookie("round", 1, time() + (86400 * 30), "/");
                echo 1;
                $round = 1;
            }
            else{
                echo $_COOKIE["round"];
                $round = $_COOKIE["round"];
            }
            echo '</span></span>: length <span id="length">';
            if(!isset($_COOKIE["length"])) {
                setcookie("length", 5, time() + (86400 * 30), "/");
                echo 5;
                $length = 5;
            }
            else{
                echo $_COOKIE["length"];
                $length = $_COOKIE["length"];
            }
            echo '</span><code class="block" id="question">';
            if (!isset($_POST["answer"])){
                if (!isset($_COOKIE["wrong"]) || $_COOKIE["wrong"]!=1){
                    $characters = 'abcdefghijklmnopqrstuvwxyz';
                    $charactersLength = strlen($characters);
                    $randomString = '';
                    $random_length = rand(1,intval($length/2));
                    for ($i = 0; $i < $random_length; $i++) {
                        $randomString .= $characters[rand(0, $charactersLength - 1)];
                    }
                    $randomString = $randomString . strrev($randomString);

                    for ($i = 0; $i < $length-2*$random_length; $i++) {
                        $random_position = rand(0,strlen($randomString)-1);
                        $random_char = $characters[rand(0,strlen($characters)-1)];
                        $randomString = substr($randomString,0,$random_position).$random_char.substr($randomString,$random_position);
                    }
                    echo $randomString;
                    setcookie("question", $randomString, time() + (86400 * 30), "/");
                }
                else {
                    echo $_COOKIE["question"];
                }
            }
            else{
                echo $_COOKIE["question"];
            }
            echo '</code></p>';
            if (isset($_POST["answer"])){
                 $answer = $_POST["answer"];
                 $question = $_COOKIE["question"];
                 function lps($str){
                    $n = strlen($str);
                    $L = array();
                    for ($i = 0; $i < $n; $i++){
                        $L[$i] = array();
                    }
                    for ($i = 0; $i < $n; $i++){
                        $L[$i][$i] = 1; 
                    }
                    for ($cl = 2; $cl <= $n; $cl++){
                        for ($i = 0; $i < $n - $cl + 1; $i++) {
                            $j = $i + $cl - 1; 
                            if ($str[$i] == $str[$j] && $cl == 2) 
                                $L[$i][$j] = 2; 
                            else if ($str[$i] == $str[$j]) 
                                $L[$i][$j] = $L[$i + 1][$j - 1] + 2; 
                            else
                                $L[$i][$j] = max($L[$i][$j - 1],$L[$i + 1][$j]); 
                        } 
                    }
                    return $L[0][$n - 1]; 
                 }
                 function minimumNumberOfDeletions($str) {
                        $n = strlen($str);
                        $len = lps($str); 
                        return ($n - $len);
                 }
                 if ($answer == minimumNumberOfDeletions($question)){
                    setcookie("round", $_COOKIE["round"]+1, time() + (86400 * 30), "/");
                    setcookie("length", $_COOKIE["length"]+5, time() + (86400 * 30), "/");
                    setcookie("wrong", 0, time() + (86400 * 30), "/");
                    echo '<p class="right" id ="response">Right!  :-)</p>
                          <form action="/palseq.php" id="r" name="r" method="post">
                            <input class="button" type="submit" name="again" id="again" value="Continue!" autofocus />
                          </form>';
                 }
                 else{
                    setcookie("wrong", 1, time() + (86400 * 30), "/");
                    echo '<p class="wrong" id="response">Wrong!  Try again...  :-(</p>
                          <form action="/palseq.php" id="r" name="r" method="post">
                            <input class="button" type="submit" name="again" id="again" value="Continue!" autofocus />
                          </form>';
                 }
            }
            else{
                echo '<form action="/palseq.php" id="f" name="f" method="post">
                What is the least number of characters you need to remove?
                <table border="0" cellspacing="3">
                    <tr>
                        <td>
                            <input type="text" class="box" name="answer" id="answer" autofocus />
                        </td>
                        <td>
                            <input type="submit" class="button" name="submit" id="submit" value="Submit!" />
                        </td>
                    </tr>
                </table>
            </form>';
            }
        }
        else {
            echo '<p class="right" id="finish">Congratulations! You passed the quiz! :-)</p>';
        }
    ?>
</body>

</html>