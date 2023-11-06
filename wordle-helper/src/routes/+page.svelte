<script>
	import AboutMe from '../components/AboutMe.svelte';
	// @ts-ignore
	import { rainbowCursor } from 'cursor-effects';
	import { onMount } from 'svelte';
	import '../app.css';

	onMount(() => {
		let docElement = document.documentElement;
		new rainbowCursor({ element: docElement });
	});
</script>

<div>
	<div class="mx-auto flex flex-wrap h-screen place-content-center">
		<h1 class="justify-self-stretch fixed top-0 w-screen lg:text-left text-center isolate">Elias Isaiah Roussos</h1>
		<AboutMe />
	</div>
</div>

<style>
	h1 {
		font-size: 1.5rem;
		display: block;
		padding: 2rem;
		background-color: lightcyan;
		border-top: 10px solid black;
		border-bottom: 10px solid black;
	}
</style>
