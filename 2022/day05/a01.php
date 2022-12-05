<?php

function solve(array $stack, array $moves, int $part = 1): string
{
	foreach ($moves as $stepNr => $step)
    {
		$moveNr = $step['move'];
		$from = $step['from'];
		$to = $step['to'];

		$elements = array_slice($stack[$from], -(int) $moveNr);
		if ($part === 1)
        {
			krsort($elements, SORT_NUMERIC);
		}
		array_splice($stack[$from], count($stack[$from]) - $moveNr, $moveNr);
		$stack[$to] = array_merge($stack[$to], $elements);
	}

	$top_layer = '';
	foreach ($stack as $row){
		$top_layer .= end($row);
	}
	return $top_layer;
}

function getStacks(string $stack): array
{
	$lines = explode("\n", $stack);
	$result = [];
	foreach ($lines as $line)
    {
		$lineCharacters = mb_str_split($line,4);
		foreach ($lineCharacters as $rowNr => $characters) 
        {
			$rowNr = $rowNr + 1;
			preg_match('/[a-zA-Z]+/', $characters, $letter);
			if (!empty($letter[0]))
            {
				!empty($result[$rowNr])
					? array_unshift($result[$rowNr], $letter[0])
					: $result[$rowNr][] = $letter[0];
			}
		}
	}

	ksort($result, SORT_NUMERIC);
	return $result;
}

function getMoves(string $moves): array
{
	$lines = explode("\n", $moves);

	$result = [];
	foreach ($lines as $lineNr => $line)
    {
		preg_match_all('/(.*?)\s[0-9]+/', $line, $orders);
		foreach ($orders[0] as $order)
        {
			list($move, $step) = explode(' ', trim($order));
			$result[$lineNr][$move] = $step;
		}
	}
	return $result;
}

list($stack, $moves) = explode("\n\n", file_get_contents('input.txt'));

$stack_array = getStacks($stack);
$moves_array = getMoves($moves);

var_dump('A - ' . solve($stack_array, $moves_array));
var_dump('B - ' . solve($stack_array, $moves_array, 2));