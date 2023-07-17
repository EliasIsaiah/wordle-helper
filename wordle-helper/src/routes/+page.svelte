<script>
	import AboutMe from '../components/AboutMe.svelte';
	import { rainbowCursor } from 'cursor-effects';
	import { onMount } from 'svelte';

	onMount(() => {
        let container = document.getElementsByClassName("container")[0];
		new rainbowCursor({ element: container });
	});
</script>

<div class="container">
	<AboutMe />

	<p>
		Created with Svelte. Visit <a href="https://kit.svelte.dev">kit.svelte.dev</a> to read the documentation
	</p>
</div>

<style>
	.container {
		display: flex;
		justify-content: center;
		align-items: center;
		height: 90vh;
	}

	.container p {
		margin: 2em;
		padding: 2em;
		background-color: oldlace;
	}
</style>
